module Action.Event.Documents.CreateEvent (CreateEvent (..), encodeCreateEvent, nameField, gameField) where

import Data.Bson (Document, Field ((:=)))
import Data.Bson qualified as Bson

import Relude

data CreateEvent = CreateEvent
    { userId :: Text
    , name :: Text
    , game :: Text
    }

userIdField :: Text -> Field
userIdField = ("userId" :=) . Bson.String

nameField :: Text -> Field
nameField = ("name" :=) . Bson.String

gameField :: Text -> Field
gameField = ("game" :=) . Bson.String

encodeCreateEvent :: CreateEvent -> Document
encodeCreateEvent createEvent =
    [ userIdField createEvent.userId
    , nameField createEvent.name
    , gameField createEvent.game
    ]