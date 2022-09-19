{-# LANGUAGE NoImplicitPrelude #-}

module Handler (AppError (..), Handler, withError, runHandler, handleError, throwError) where

import Control.Monad.Logger (LoggingT)
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString.Lazy as LBS
import Env (Env)
import Network.HTTP.Types (Status)
import Network.Wai (Response)
import qualified Network.Wai as Wai
import Relude

data AppError = AppError {log :: Maybe Text, status :: Status, response :: LBS.ByteString}

type Handler = ReaderT Env (ExceptT AppError (LoggingT IO))

throwError :: MonadTrans m => AppError -> m Handler a
throwError = withError . Left

withError :: MonadTrans m => Either AppError a -> m Handler a
withError = lift . lift . ExceptT . pure

runHandler :: Env -> Handler Response -> IO Response
runHandler env = Logger.runStdoutLoggingT . handleError . runExceptT . usingReaderT env

handleError :: LoggingT IO (Either AppError Response) -> LoggingT IO Response
handleError withErr =
    withErr
        >>= either
            ( \err -> do
                maybe (pure ()) Logger.logErrorN $ log err
                pure $ Wai.responseLBS (status err) mempty $ response err
            )
            pure