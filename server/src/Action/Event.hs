module Action.Event (listEvents) where

import Action (Action)
import Action qualified
import Action.Auth.Documents.User (User (..))
import Data.Text.Lazy qualified as LazyText
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

listEvents :: User -> ActionT LazyText.Text Action ()
listEvents user =
    do
        logger <- Action.createLogger "listEvents"

        Action.logDebug logger $ "fetching events for " <> show user.userId
        ScottyT.html $ LazyText.pack "<h1>hello there</h1>"