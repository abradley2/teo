module App (
    server,
) where

import Action (ActionM)
import Action qualified
import Action.Auth qualified
import Action.Hello qualified
import Data.Text.Lazy qualified as LazyText
import Env (Env (..))
import Env qualified
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (Policy)
import Network.Wai.Middleware.Static qualified as Static
import Relude
import Web.Scotty.Trans (ScottyT)
import Web.Scotty.Trans qualified as ScottyT

staticMiddleware :: Middleware
staticMiddleware =
    Static.staticPolicy $
        serveApp <> Static.addBase "public" <> Static.noDots <> Static.addBase "../web"

serveApp :: Policy
serveApp = Static.policy defaultIndex
  where
    defaultIndex "" = Just "index.html"
    defaultIndex ('a' : 'p' : 'p' : _) = Just "index.html"
    defaultIndex s = Just s

server :: IO ()
server = do
    env <- runExceptT Env.getEnv
    either putStrLn (\e -> application e >>= run (Env.port e)) env

handler :: ScottyT LazyText.Text ActionM ()
handler = do
    ScottyT.get "/api/hello" Action.Hello.sayHello
    ScottyT.post "/api/login" Action.Auth.authorize
    ScottyT.get "/api/check-auth" Action.Auth.checkAuth

application :: Env -> IO Application
application env =
    do
        putStrLn $ "Running server on port " <> show (port env)
        staticMiddleware <$> ScottyT.scottyAppT (Action.runHandler env) handler
