{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Action.Auth (withAuth, authorize, AuthToken (..)) where

import qualified Action.Cookie
import qualified Control.Monad.Logger as Logger
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified Database.Redis as Redis
import qualified Env
import Handler (AppError (..), Handler)
import qualified Handler
import Network.HTTP.Types (status400, status500)
import Relude
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT

newtype AuthToken = AuthToken Text

withAuth :: (AuthToken -> ActionT LazyText.Text Handler a) -> ActionT LazyText.Text Handler a
withAuth handler = do
    redisConn <- asks Env.redisConn
    lift $ Logger.logDebugN "Invoking handler with auth"
    clientToken <-
        Action.Cookie.readCookie "auth"
            >>= Handler.withError
                . maybeToRight
                    ( AppError
                        { log = Nothing
                        , response = "User is not authorized"
                        , status = status400
                        }
                    )
    lift $ Logger.logDebugN "Retrieved auth cookie"
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