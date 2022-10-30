module Action (
    AppError (..),
    Action,
    Logger,
    LoggingContext,
    withError,
    withError',
    withHandledError,
    withHandledError',
    runAction,
    runHandlerAction,
    getLoggingContext,
    createLogger,
    createLogger',
    throwError,
    withMongoAction,
    withMongoAction',
    withRedisAction,
    withRedisAction',
    logDebug',
    logDebug,
    logError',
    logError,
    logWarn',
    logWarn,
) where

import Control.Monad.Catch (try)
import Control.Monad.Except (liftEither)
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

type Action = ExceptT AppError (LoggingT (ReaderT Env IO))

type Logger = Logger.LogLevel -> Text -> Action ()

newtype ClientId = ClientId (Maybe Text)

newtype LoggingContext = LoggingContext ClientId

getLoggingContext :: ActionT LazyText.Text Action LoggingContext
getLoggingContext = LoggingContext . ClientId . fmap LazyText.toStrict <$> ScottyT.header "X-Client-Id"

createLogger :: Text -> ActionT LazyText.Text Action Logger
createLogger source = createLogger' source <$> getLoggingContext

createLogger' :: Text -> LoggingContext -> Logger
createLogger' source (LoggingContext (ClientId clientId)) level logStr = do
    lift $ LoggingT $ \f -> ReaderT $ \env ->
        let logMsg = LogMsg clientId env.requestId logStr
         in f Logger.defaultLoc source level $ show $ Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode logMsg

logDebug :: Logger -> Text -> ActionT LazyText.Text Action ()
logDebug logger = lift . logDebug' logger

logDebug' :: Logger -> Text -> Action ()
logDebug' logger = logger Logger.LevelDebug

logError :: Logger -> Text -> ActionT LazyText.Text Action ()
logError logger = lift . logError' logger

logError' :: Logger -> Text -> Action ()
logError' logger = logger Logger.LevelError

logWarn' :: Logger -> Text -> Action ()
logWarn' logger = logger Logger.LevelWarn

logWarn :: Logger -> Text -> ActionT LazyText.Text Action ()
logWarn logger = lift . logWarn' logger

withError :: Logger -> (b -> AppError) -> Either b a -> ActionT LazyText.Text Action a
withError logger toErr e = do
    lift $ withError' logger toErr e

withError' :: Logger -> (b -> AppError) -> Either b a -> Action a
withError' logger toErr e = do
    let result = first toErr e
    case result of
        Left err ->
            let logLevel = if err.status == status500 then Logger.LevelError else Logger.LevelDebug
             in maybe (pure ()) (logger logLevel) err.log
        Right _ -> pure ()

    ExceptT $ pure result

withHandledError :: Either AppError a -> ActionT LazyText.Text Action a
withHandledError = lift . withHandledError'

withHandledError' :: Either AppError a -> Action a
withHandledError' = liftEither

withRedisAction :: Redis.Redis a -> ActionT LazyText.Text Action a
withRedisAction action = lift $ withRedisAction' action

withRedisAction' :: Redis.Redis a -> Action a
withRedisAction' action = do
    redisConn <- asks (\e -> e.redisConn)
    liftIO . Redis.runRedis redisConn $ action

withMongoAction :: MongoDb.Action Action a -> ActionT LazyText.Text Action (Either MongoDb.Failure a)
withMongoAction = lift . withMongoAction'

withMongoAction' :: MongoDb.Action Action a -> Action (Either MongoDb.Failure a)
withMongoAction' action = do
    pipe <- asks (\e -> e.mongoPipe)
    try $ MongoDb.access pipe MongoDb.master "public_db" action

throwError :: Logger -> AppError -> ActionT LazyText.Text Action a
throwError logger appError = withError logger (const appError) (Left appError)

runAction :: Env -> Action a -> IO (Either AppError a)
runAction env action = runReaderT (Logger.runStdoutLoggingT (runExceptT action)) env

runHandlerAction :: Env -> Action Response -> IO Response
runHandlerAction env handler = do
    requestId <- show <$> UUID.V4.nextRandom
    fmap
        (Wai.mapResponseHeaders (("X-Request-Id", Text.encodeUtf8 requestId) :))
        . usingReaderT (env{Env.requestId = Just requestId})
        . Logger.runStdoutLoggingT
        . handleError
        . runExceptT
        $ handler
  where
    handleError :: LoggingT (ReaderT Env IO) (Either AppError Response) -> LoggingT (ReaderT Env IO) Response
    handleError withErr = do
        result <- withErr
        case result of
            Left err -> do
                pure $ Wai.responseLBS (err.status) [] err.response
            Right res ->
                pure res
