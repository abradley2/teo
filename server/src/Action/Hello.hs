module Action.Hello (sayHello) where

import Control.Monad.Logger qualified as Logger
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Handler (Handler)
import Handler qualified
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

sayHello :: ActionT LazyText.Text Handler ()
sayHello = do
    lift $ Logger.logDebugN $ Text.pack "Say hello called"
    a <- Handler.withError $ Right ""
    ScottyT.html $ LazyText.pack "<h1>hello there</h1>"