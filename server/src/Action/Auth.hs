module Action.Auth (withAuth, authorize, AuthToken (..), checkAuth) where

import Action.Cookie qualified
import Control.Monad.Logger qualified as Logger
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Database.Redis qualified as Redis
import Env qualified
import Handler (AppError (..), Handler)
import Handler qualified
import Network.HTTP.Types (status400, status500)
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

newtype AuthToken = AuthToken Text

authCookieName :: Text
authCookieName = "auth"

withAuth :: (AuthToken -> ActionT LazyText.Text Handler a) -> ActionT LazyText.Text Handler a
withAuth handler =
    let logger = Handler.withLogger "withAuth"
     in do
            redisConn <- asks Env.redisConn
            logger Logger.LevelDebug "Invoking handler with auth"
            clientToken <-
                Action.Cookie.readCookie authCookieName
                    >>= Handler.withError
                        . maybeToRight
                            ( AppError
                                { log = Nothing
                                , response = "User is not authorized"
                                , status = status400
                                }
                            )
            logger Logger.LevelDebug "Retrieved auth cookie"
            authTokenResult <-
                liftIO
                    . Redis.runRedis redisConn
                    . Redis.get
                    $ Text.encodeUtf8 clientToken
            authToken <-
                Handler.withError $
                    first
                        ( \err ->
                            AppError
                                { log = Just $ "Failed to retrieve auth token from Redis: " <> show err
                                , response = "Internal server error"
                                , status = status500
                                }
                        )
                        authTokenResult
            maybe
                ( Handler.withError $
                    Left $
                        AppError
                            { log = Nothing
                            , response = "User token not found"
                            , status = status400
                            }
                )
                (handler . AuthToken . Text.decodeUtf8)
                authToken

authorize :: ActionT LazyText.Text Handler ()
authorize = do
    lift $ Logger.logDebugN "Attempting to authorize user"
    redisConn <- asks Env.redisConn

    clientToken <- UUID.toText <$> liftIO UUID.V4.nextRandom
    authToken <- UUID.toText <$> liftIO UUID.V4.nextRandom

    storeTokenResult <-
        liftIO . Redis.runRedis redisConn $
            Redis.set (Text.encodeUtf8 clientToken) (Text.encodeUtf8 authToken)
                >>= const (Redis.expire (Text.encodeUtf8 clientToken) 3600)

    Handler.withError
        ( first
            ( \err ->
                AppError
                    { log = Just $ "Failed to store auth token in Redis: " <> show err
                    , response = "Internal server error"
                    , status = status500
                    }
            )
            storeTokenResult
        )
        >>= const
            ( ScottyT.json clientToken
            )

newtype CheckAuthResponse = CheckAuthResponse {authorized :: Bool}

instance ToJSON CheckAuthResponse where
    toJSON res =
        Aeson.object
            [ ("authorized", Aeson.toJSON $ authorized res)
            ]

checkAuth :: ActionT LazyText.Text Handler ()
checkAuth = do
    lift $ Logger.logDebugN "Checking auth status"
    redisConn <- asks Env.redisConn

    authCookie <- Action.Cookie.readCookie authCookieName

    case authCookie of
        Just clientToken -> do
            lift $ Logger.logDebugN "User has auth cookie, querying for token from redis"
            authTokenQuery <-
                liftIO
                    . Redis.runRedis redisConn
                    . Redis.get
                    $ Text.encodeUtf8 clientToken
            authTokenResult <-
                Handler.withError $
                    first
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
            lift $ Logger.logDebugN "User doesn't have auth token, verified that they are not authorized"
            ScottyT.json $ CheckAuthResponse{authorized = False}
