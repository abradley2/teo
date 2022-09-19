{-# LANGUAGE NoImplicitPrelude #-}

module Action.Hello (sayHello) where

import qualified Control.Monad.Logger as Logger
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Handler (Handler)
import qualified Handler
import Relude
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT

sayHello :: ActionT LazyText.Text Handler ()
sayHello = do
    lift $ Logger.logDebugN $ Text.pack "Say hello called"
    a <- Handler.withError $ Right ""
    ScottyT.html $ LazyText.pack "<h1>hello there</h1>"