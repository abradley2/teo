{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Action.Auth (withAuth, authorize, AuthToken (..), checkAuth) where

import Action (ActionM, AppError (..))
import Action qualified
import Action.Cookie qualified
import Control.Monad.Logger qualified as Logger
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
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
    logger Logger.LevelDebug "Attempting to authorize user"

    redisConn <- asks Env.redisConn

    clientToken <- UUID.toText <$> liftIO UUID.V4.nextRandom
    authToken <- UUID.toText <$> liftIO UUID.V4.nextRandom

    storeTokenResult <-
        liftIO . Redis.runRedis redisConn $
            Redis.set (Text.encodeUtf8 clientToken) (Text.encodeUtf8 authToken)
                >>= const (Redis.expire (Text.encodeUtf8 clientToken) 3600)

    either (const $ pure ()) (const $ Action.Cookie.setCookie authCookieName clientToken) storeTokenResult

    Action.withError
        logger
        ( \err ->
            AppError
                { log = Just $ "Failed to store auth token in Redis: " <> show err
                , response = "Internal server error"
                , status = status500
                }
        )
        storeTokenResult
        >>= const
            ( ScottyT.json $ CheckAuthResponse True
            )

newtype CheckAuthResponse = CheckAuthResponse {authorized :: Bool}

instance ToJSON CheckAuthResponse where
    toJSON res =
        Aeson.object
            [ ("authorized", Aeson.toJSON $ authorized res)
            ]

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
            ScottyT.json $ CheckAuthResponse{authorized = isJust authTokenResult}
        Nothing -> do
            logger Logger.LevelDebug "User doesn't have auth token, verified that they are not authorized"
            ScottyT.json $ CheckAuthResponse{authorized = False}
