{-# LANGUAGE NoImplicitPrelude #-}

module App (
    server,
) where

import qualified Action.Hello
import qualified Data.Text.Lazy as LazyText
import Env (Env (..))
import qualified Env
import Handler (Handler)
import qualified Handler
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (Policy)
import qualified Network.Wai.Middleware.Static as Static
import Relude
import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as ScottyT

staticMiddleware :: Middleware
staticMiddleware =
    Static.staticPolicy $
        serveApp <> Static.addBase "public" <> Static.noDots

serveApp :: Policy
serveApp = Static.policy defaultIndex
  where
    defaultIndex "" = Just "index.html"
    defaultIndex ('a' : 'p' : 'p' : _) = Just "index.html"
    defaultIndex s = Just s

server :: IO ()
server = do
    env <- runExceptT Env.getEnv
    either putStrLn (application >=> run 8080) env

handler :: ScottyT LazyText.Text Handler ()
handler = do
    ScottyT.get (fromString "/api/hello") Action.Hello.sayHello

application :: Env -> IO Application
application env = staticMiddleware <$> ScottyT.scottyAppT (Handler.runHandler env) handler
