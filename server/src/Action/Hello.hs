module Action.Hello (sayHello) where

import Action (Action)
import Action qualified
import Data.Text.Lazy qualified as LazyText
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

sayHello :: ActionT LazyText.Text Action ()
sayHello =
    do
        logger <- Action.createLogger "sayHello"
        Action.logDebug logger "saying hello"
        ScottyT.html $ LazyText.pack "<h1>hello there</h1>"