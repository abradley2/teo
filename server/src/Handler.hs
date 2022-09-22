module Handler (AppError (..), Handler, withError, runHandler, handleError, AppLogger, withLogger) where

import Control.Monad.Logger (LoggingT (LoggingT))
import Control.Monad.Logger qualified as Logger
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as Text
import Data.UUID.V4 qualified as UUID.V4
import Env (Env)
import Env qualified
import Network.HTTP.Types (Status)
import Network.Wai (Response)
import Network.Wai qualified as Wai
import Relude

data AppError = AppError {log :: Maybe Logger.LogStr, status :: Status, response :: LBS.ByteString}

newtype AppLogger a = AppLogger {runLogger :: LoggingT (ReaderT Env IO) a} deriving (Applicative, Functor, Monad, MonadIO)

type Handler = ExceptT AppError AppLogger

appLog :: Text -> Logger.LogLevel -> Logger.LogStr -> AppLogger ()
appLog source level logStr = AppLogger $ LoggingT $ \f -> ReaderT $ \env -> f Logger.defaultLoc source level logStr

withLogger :: MonadTrans m => Text -> Logger.LogLevel -> Logger.LogStr -> m Handler ()
withLogger source level logStr = lift . lift $ appLog source level logStr

withError :: MonadTrans m => Either AppError a -> m Handler a
withError = lift . ExceptT . AppLogger . pure

runHandler :: Env -> Handler Response -> IO Response
runHandler env handler = do
    requestId <- show <$> UUID.V4.nextRandom
    fmap
        (Wai.mapResponseHeaders (("X-Request-Id", Text.encodeUtf8 requestId) :))
        . usingReaderT (env{Env.requestId = Just requestId})
        . Logger.runStdoutLoggingT
        . handleError
        . runLogger
        . runExceptT
        $ handler

handleError :: LoggingT (ReaderT Env IO) (Either AppError Response) -> LoggingT (ReaderT Env IO) Response
handleError withErr = do
    result <- withErr
    case result of
        Left err -> do
            case log err of
                Just logMsg -> runLogger $ appLog "" Logger.LevelError logMsg
                Nothing -> pure ()
            pure $ Wai.responseLBS (status err) [] (response err)
        Right response ->
            pure response
