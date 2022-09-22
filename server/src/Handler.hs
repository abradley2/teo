module Handler (AppError (..), Handler, withError, runHandler, handleError, throwError, Logger) where

import Control.Monad.Logger (LoggingT (..))
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

type NewHandler = ExceptT AppError AppLogger

newtype AppLogger a = Logger {runLogger :: LoggingT (ReaderT Env IO) a}

logDebug :: Logger.LogLevel -> LoggingT (ReaderT Env IO) ()
logDebug level = LoggingT $ \f -> ReaderT $ \env -> Logger.runStderrLoggingT $ f Logger.defaultLoc "" level

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