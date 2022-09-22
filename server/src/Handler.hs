module Handler (AppError (..), Handler, withError, runHandler, handleError, throwError, AppLogger) where

import Control.Monad.Logger (LoggingT (LoggingT))
import Control.Monad.Logger qualified as Logger
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.UUID.V4 qualified as UUID.V4
import Env (Env)
import Env qualified
import Network.HTTP.Types (Status)
import Network.Wai (Response)
import Network.Wai qualified as Wai
import Relude
import Web.Scotty.Trans (ActionT)

data AppError = AppError {log :: Maybe Text, status :: Status, response :: LBS.ByteString}

type Handler = ReaderT Env (ExceptT AppError (LoggingT IO))

type AppHandler = ExceptT AppError (AppLogger IO)

newtype AppLogger m a = AppLogger {runLogger :: LoggingT (ReaderT Env m) a}

-- appLog :: Text -> Logger.LogLevel -> Logger.LogStr -> LoggingT (ReaderT Env IO) ()
appLog :: Text -> Logger.LogLevel -> Logger.LogStr -> AppLogger IO ()
appLog source level logStr = AppLogger $ LoggingT $ \f -> ReaderT $ \env -> f Logger.defaultLoc source level logStr

throwError :: MonadTrans m => AppError -> m Handler a
throwError = withError . Left

withError :: MonadTrans m => Either AppError a -> m Handler a
withError = lift . lift . ExceptT . pure

runHandler :: Env -> Handler Response -> IO Response
runHandler env handler = do
    requestId <- show <$> UUID.V4.nextRandom
    fmap
        (Wai.mapResponseHeaders (("X-Request-Id", Text.encodeUtf8 requestId) :))
        . Logger.runStdoutLoggingT
        . handleError
        . runExceptT
        . usingReaderT (env{Env.requestId = Just requestId})
        $ handler

handleError :: LoggingT IO (Either AppError Response) -> LoggingT IO Response
handleError withErr =
    withErr
        >>= either
            ( \err -> do
                maybe (pure ()) Logger.logErrorN $ log err
                pure $ Wai.responseLBS (status err) mempty $ response err
            )
            pure