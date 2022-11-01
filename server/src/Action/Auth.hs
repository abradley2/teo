module Action.Auth (withAuth, authorize, checkAuth) where

import Action (Action, AppError (..), LoggingContext)
import Action qualified
import Action.Auth.AuthorizeRequest (AuthorizeRequest (..))
import Action.Auth.AuthorizeResponse (AuthorizeResponse (..))
import Action.Auth.CheckAuthResponse (CheckAuthResponse (..))
import Action.Auth.Documents.User (User)
import Action.Auth.Documents.User qualified as Documents.User
import Action.Cookie qualified
import Control.Concurrent.Async qualified as Async
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Time.Clock.POSIX qualified as POSIX
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Database.MongoDB.Query qualified as MQuery
import Database.Redis qualified as Redis
import Env qualified
import Network.HTTP.Types (status400, status401, status500)
import Relude
import Web.JWT (EncodeSigner (..), JOSEHeader (..), JWTClaimsSet (..))
import Web.JWT qualified as JWT
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

authCookieName :: Text
authCookieName = "auth"

createJWT :: User -> Action Text
createJWT user = do
    let header =
            JOSEHeader
                { alg = Just JWT.HS256
                , typ = Just "JWT"
                , kid = Nothing
                , cty = Nothing
                }

    signer <- EncodeHMACSecret <$> asks (\e -> e.jwtSigningKey)
    iatDate <- liftIO POSIX.getPOSIXTime
    expDate <- liftIO $ (+ 86400) <$> POSIX.getPOSIXTime
    pure $
        JWT.encodeSigned
            signer
            header
            ( JWTClaimsSet
                { iss = Nothing
                , jti = Nothing
                , iat = JWT.numericDate iatDate
                , nbf = Nothing
                , exp = JWT.numericDate expDate
                , aud = Left <$> JWT.stringOrURI "teo-web-ogsac"
                , sub = JWT.stringOrURI user.userId
                , unregisteredClaims =
                    JWT.ClaimsMap
                        ( mempty
                            & Map.insert "userId" (Aeson.String user.userId)
                        )
                }
            )

withAuth :: (User -> ActionT LazyText.Text Action a) -> ActionT LazyText.Text Action a
withAuth handler =
    do
        logger <- Action.createLogger "Action.Auth.withAuth"

        Action.logDebug logger "Invoking handler with auth"

        clientToken <-
            Action.Cookie.readCookie authCookieName
                >>= ( Action.withError
                        logger
                        ( const
                            ( AppError
                                { log = Nothing
                                , response = "User is not authorized"
                                , status = status400
                                }
                            )
                        )
                        . maybeToRight (Text.pack "")
                    )

        Action.logDebug logger "Retrieved auth cookie, checking for userId in redis"

        userId <-
            Action.withRedisAction
                ( Redis.get $ Text.encodeUtf8 clientToken
                )
                >>= Action.withError
                    logger
                    ( \err ->
                        AppError
                            { log = Just $ "Failed to retrieve auth token from Redis: " <> show err
                            , response = "Internal server error"
                            , status = status500
                            }
                    )
                >>= ( Action.withError
                        logger
                        ( const $
                            AppError
                                { log = Just "No auth token found in redis"
                                , response = "Session not found, please login again"
                                , status = status401
                                }
                        )
                        . maybeToRight ()
                    )

        Action.logDebug logger "Retrieved userId from redis, retrieving user doc from mongo"

        user <-
            Action.withMongoAction
                ( do
                    MQuery.findOne $
                        MQuery.select
                            [ Documents.User.userIdField (Text.decodeUtf8 userId)
                            ]
                            Documents.User.collection
                )
                >>= Action.withError
                    logger
                    ( \err ->
                        AppError
                            { log = Just $ "Failed to retrieve user from Mongo: " <> show err
                            , response = "Internal server error"
                            , status = status500
                            }
                    )
                >>= ( Action.withError
                        logger
                        ( const $
                            AppError
                                { log = Just "No user found in mongo"
                                , response = "Session not found, please login again"
                                , status = status401
                                }
                        )
                        . maybeToRight ()
                    )
                >>= ( Action.withError
                        logger
                        ( \err ->
                            AppError
                                { log = Just $ "User doc found but failed to decode: " <> err
                                , response = "Internal server error"
                                , status = status500
                                }
                        )
                        . Documents.User.decodeUser
                    )

        Action.logDebug logger "Retrieved user doc from mongo, invoking handler with user"

        handler user

authorize :: ActionT LazyText.Text Action ()
authorize = do
    env <- ask
    ctx <- Action.getLoggingContext

    let logger = Action.createLogger' "Action.Auth.authorize" ctx

    let parseBody :: LazyByteString.ByteString -> Either String AuthorizeRequest
        parseBody = Aeson.eitherDecode

    Action.logDebug logger "Attempting to authorize user"

    reqBody <-
        ScottyT.body
            >>= Action.withError
                logger
                ( \err ->
                    AppError
                        { log = Just $ "Failed to parse request body: " <> Text.pack err
                        , response = "Invalid request body"
                        , status = status400
                        }
                )
                . parseBody

    clientToken <- UUID.toText <$> liftIO UUID.V4.nextRandom

    let storeTokenInRedis =
            Action.withRedisAction'
                ( Redis.set (Text.encodeUtf8 clientToken) (Text.encodeUtf8 reqBody.userId)
                    >>= const (Redis.expire (Text.encodeUtf8 clientToken) 3600)
                )
                >>= Action.withError'
                    logger
                    ( \err ->
                        AppError
                            { log = Just $ "Failed to store auth token in Redis: " <> show err
                            , response = "Internal server error"
                            , status = status500
                            }
                    )

    (userResult, redisResult) <- liftIO $
        Async.withAsync (Action.runAction env $ getOrCreateProfile ctx reqBody.userId) $ \a1 -> do
            Async.withAsync (Action.runAction env storeTokenInRedis) $ \a2 -> do
                Async.waitBoth a1 a2

    user <- Action.withHandledError userResult
    _ <- Action.withHandledError redisResult

    Action.Cookie.setCookie authCookieName clientToken

    token <- lift $ createJWT user

    ScottyT.json $ AuthorizeResponse reqBody.userId token

checkAuth :: ActionT LazyText.Text Action ()
checkAuth = do
    ctx <- Action.getLoggingContext
    let logger = Action.createLogger' "Action.Auth.checkAuth" ctx

    Action.logDebug logger "Checking auth status"

    authCookie <- Action.Cookie.readCookie authCookieName

    case authCookie of
        Just clientToken -> do
            Action.logDebug logger "User has auth cookie, querying for token from redis"
            authTokenQuery <-
                Action.withRedisAction
                    ( Redis.get $
                        Text.encodeUtf8 clientToken
                    )
            authTokenResult <-
                Action.withError
                    logger
                    ( \err ->
                        AppError
                            { log = Just $ "Failed to retrieve auth token from Redis: " <> show err
                            , response = "Internal server error"
                            , status = status500
                            }
                    )
                    authTokenQuery

            user <- case authTokenResult of
                Just userId -> lift $ getProfileById ctx (Text.decodeUtf8 userId)
                Nothing -> pure Nothing

            token <-
                case user of
                    Just u -> lift $ Just <$> createJWT u
                    _ -> pure Nothing

            ScottyT.json $ CheckAuthResponse ((.userId) <$> user) token
        Nothing -> do
            Action.logDebug
                logger
                "User doesn't have auth token, verified that they are not authorized"
            ScottyT.json $ CheckAuthResponse Nothing Nothing

getOrCreateProfile :: LoggingContext -> Text -> Action User
getOrCreateProfile ctx userId =
    do
        let logger = Action.createLogger' "Action.Auth.getOrCreateProfile" ctx

        Action.logDebug' logger "Updating or creating user profile in mongo"

        user <-
            ( fromMaybe $
                    Documents.User.User
                        { _id = Nothing
                        , userId = userId
                        }
                )
                <$> getProfileById ctx userId

        _ <-
            Action.withMongoAction'
                ( MQuery.save
                    Documents.User.collection
                    ( Documents.User.encodeUser user
                    )
                )
                >>= Action.withError'
                    logger
                    ( \err ->
                        AppError
                            { log = Just $ "Failed to get or create user profile: " <> show err
                            , response = "Internal server error"
                            , status = status500
                            }
                    )
        Action.logDebug' logger "Successfully updated or created user profile in mongo"
        pure user

getProfileById :: LoggingContext -> Text -> Action (Maybe User)
getProfileById ctx userId = do
    let logger = Action.createLogger' "Action.Auth.getProfileById" ctx

    existingUserDoc <-
        Action.withMongoAction'
            ( MQuery.findOne $
                MQuery.select
                    [ Documents.User.userIdField userId
                    ]
                    Documents.User.collection
            )
            >>= Action.withError'
                logger
                ( \err ->
                    AppError
                        { log = Just $ "Query to search for existing user failed: " <> show err
                        , response = "Internal server error"
                        , status = status500
                        }
                )

    case Documents.User.decodeUser <$> existingUserDoc of
        Nothing -> pure Nothing
        Just (Right user) -> pure (Just user)
        Just (Left err) ->
            Action.withError'
                logger
                ( const $
                    AppError
                        { log =
                            Just $
                                "Failed to decode existing user for userId "
                                    <> userId
                                    <> ": "
                                    <> err
                        , response = "Internal server error"
                        , status = status500
                        }
                )
                (Left err)
