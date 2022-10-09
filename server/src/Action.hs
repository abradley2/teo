module Action (AppError (..), ActionM, withError, runHandler, handleError, createLogger, throwError, withMongoAction, withRedisAction) where

import Control.Monad.Catch (handle)
import Control.Monad.Logger (LoggingT (LoggingT))
import Control.Monad.Logger qualified as Logger
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.UUID.V4 qualified as UUID.V4
import Database.MongoDB qualified as MongoDb
import Database.Redis qualified as Redis
import Env (Env)
import Env qualified
import Network.HTTP.Types (Status, status500)
import Network.Wai (Response)
import Network.Wai qualified as Wai
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

data AppError = AppError {log :: Maybe Text, status :: Status, response :: LBS.ByteString}

data LogMsg = LogMsg
    { _clientId :: Maybe Text
    , _requestId :: Maybe Text
    , _logMsg :: Text
    }

instance Aeson.ToJSON LogMsg where
    toJSON logMsg =
        Aeson.object
            [ ("clientId", Aeson.toJSON logMsg._clientId)
            , ("requestId", Aeson.toJSON logMsg._requestId)
            , ("logMsg", Aeson.toJSON logMsg._logMsg)
            ]

type ActionM = ExceptT AppError (LoggingT (ReaderT Env IO))

type Logger = Logger.LogLevel -> Text -> ActionT LazyText.Text ActionM ()

createLogger :: Text -> Logger.LogLevel -> Text -> ActionT LazyText.Text ActionM ()
createLogger source level logStr = do
    clientId <- fmap LazyText.toStrict <$> ScottyT.header "X-Client-Id"
    lift . lift $ LoggingT $ \f -> ReaderT $ \env ->
        let logMsg = LogMsg clientId env.requestId logStr
         in f Logger.defaultLoc source level $ show $ Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode logMsg

withError :: Logger -> (b -> AppError) -> Either b a -> ActionT LazyText.Text ActionM a
withError logger toErr e = do
    let result = first toErr e
    case result of
        Left err ->
            let logLevel = if err.status == status500 then Logger.LevelError else Logger.LevelDebug
             in maybe (pure ()) (logger logLevel) err.log
        Right _ ->
            pure ()

    lift . ExceptT $ pure result

withRedisAction :: Redis.Redis a -> ActionT LazyText.Text ActionM a
withRedisAction action = do
    redisConn <- asks Env.redisConn
    liftIO . Redis.runRedis redisConn $ action

withMongoAction :: MongoDb.Action (ActionT LazyText.Text ActionM) a -> ActionT LazyText.Text ActionM (Either MongoDb.Failure a)
withMongoAction action = do
    pipe <- asks Env.mongoPipe
    handle catch $ Right <$> MongoDb.access pipe MongoDb.UnconfirmedWrites "public_db" action
  where
    catch :: MongoDb.Failure -> ActionT LazyText.Text ActionM (Either MongoDb.Failure a)
    catch = pure . Left

throwError :: Logger -> AppError -> ActionT LazyText.Text ActionM a
throwError logger appError = withError logger (const appError) (Left appError)

runHandler :: Env -> ActionM Response -> IO Response
runHandler env handler = do
    requestId <- show <$> UUID.V4.nextRandom
    fmap
        (Wai.mapResponseHeaders (("X-Request-Id", Text.encodeUtf8 requestId) :))
        . usingReaderT (env{Env.requestId = Just requestId})
        . Logger.runStdoutLoggingT
        . handleError
        . runExceptT
        $ handler

handleError :: LoggingT (ReaderT Env IO) (Either AppError Response) -> LoggingT (ReaderT Env IO) Response
handleError withErr = do
    result <- withErr
    case result of
        Left err -> do
            pure $ Wai.responseLBS (status err) [] err.response
        Right res ->
            pure res
