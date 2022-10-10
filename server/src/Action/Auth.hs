module Action.Auth (withAuth, authorize, AuthToken (..), checkAuth) where

import Action (ActionM, AppError (..))
import Action qualified
import Action.Auth.AuthorizeRequest (AuthorizeRequest (..))
import Action.Auth.AuthorizeResponse (AuthorizeResponse (..))
import Action.Auth.CheckAuthResponse (CheckAuthResponse (..))
import Action.Cookie qualified
import Control.Concurrent.Async qualified as Async
import Control.Monad.Logger qualified as Logger
import Data.Aeson qualified as Aeson
import Data.Bson (Field ((:=)))
import Data.Bson qualified as Bson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Database.MongoDB.Query qualified as MQuery
import Database.Redis qualified as Redis
import Env qualified
import Network.HTTP.Types (status400, status500)
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

newtype AuthToken = AuthToken Text

authCookieName :: Text
authCookieName = "auth"

withAuth :: (AuthToken -> ActionT LazyText.Text ActionM a) -> ActionT LazyText.Text ActionM a
withAuth handler =
    let logger = Action.createLogger "Action.Auth.withAuth"
     in do
            redisConn <- asks Env.redisConn
            logger Logger.LevelDebug "Invoking handler with auth"
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
            logger Logger.LevelDebug "Retrieved auth cookie"
            authTokenResult <-
                liftIO
                    . Redis.runRedis redisConn
                    . Redis.get
                    $ Text.encodeUtf8 clientToken
            authToken <-
                Action.withError
                    logger
                    ( \err ->
                        AppError
                            { log = Just $ "Failed to retrieve auth token from Redis: " <> show err
                            , response = "Internal server error"
                            , status = status500
                            }
                    )
                    authTokenResult
            maybe
                ( Action.throwError logger $
                    AppError
                        { log = Nothing
                        , response = "User token not found"
                        , status = status400
                        }
                )
                (handler . AuthToken . Text.decodeUtf8)
                authToken

authorize :: ActionT LazyText.Text ActionM ()
authorize = do
    let logger = Action.createLogger "Action.Auth.authorize"

    let parseBody :: LazyByteString.ByteString -> Either String AuthorizeRequest
        parseBody = Aeson.eitherDecode

    logger Logger.LevelDebug "Attempting to authorize user"

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
    authToken <- UUID.toText <$> liftIO UUID.V4.nextRandom

    _ <-
        Action.withRedisAction
            ( Redis.set (Text.encodeUtf8 clientToken) (Text.encodeUtf8 authToken)
                >>= const (Redis.expire (Text.encodeUtf8 clientToken) 3600)
            )
            >>= Action.withError
                logger
                ( \err ->
                    AppError
                        { log = Just $ "Failed to store auth token in Redis: " <> show err
                        , response = "Internal server error"
                        , status = status500
                        }
                )

    _ <- liftIO $ Async.withAsync (pure $ getOrCreateProfile reqBody.userId) $ \a1 -> do
        res <- Async.wait a1
        pure res

    Action.Cookie.setCookie authCookieName clientToken

    ScottyT.json $ AuthorizeResponse reqBody.userId True

checkAuth :: ActionT LazyText.Text ActionM ()
checkAuth = do
    let logger = Action.createLogger "Action.Auth.checkAuth"

    logger Logger.LevelDebug "Checking auth status"
    redisConn <- asks Env.redisConn

    authCookie <- Action.Cookie.readCookie authCookieName

    case authCookie of
        Just clientToken -> do
            logger Logger.LevelDebug "User has auth cookie, querying for token from redis"
            authTokenQuery <-
                liftIO
                    . Redis.runRedis redisConn
                    . Redis.get
                    $ Text.encodeUtf8 clientToken
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
            ScottyT.json $ CheckAuthResponse (isJust authTokenResult)
        Nothing -> do
            logger Logger.LevelDebug "User doesn't have auth token, verified that they are not authorized"
            ScottyT.json $ CheckAuthResponse False

getOrCreateProfile :: Text -> ActionT LazyText.Text ActionM Int
getOrCreateProfile userId = do
    let logger = Action.createLogger "Action.Auth.getOrCreateProfile"
    _ <-
        Action.withMongoAction
            ( MQuery.upsert
                ( MQuery.select
                    [ "userId" := Bson.String userId
                    ]
                    "users"
                )
                [ "userId" := Bson.String userId
                ]
            )
            >>= Action.withError
                logger
                ( \err ->
                    AppError
                        { log = Just $ "Failed to get or create user profile: " <> show err
                        , response = "Internal server error"
                        , status = status500
                        }
                )

    logger Logger.LevelDebug "Successfully retrieved or created user profile"

    pure 1
