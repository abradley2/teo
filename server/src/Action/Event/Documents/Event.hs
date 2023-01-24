module Action.Event.Documents.Event (Event (..), decodeEvent, collection, userIdField, nameField, gameField) where

import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Bson (Document, Field ((:=)))
import Data.Bson qualified as Bson
import Database.MongoDB (Collection)
import Relude

data Event = Event
    { _id :: Text
    , userId :: Text
    , name :: Text
    , game :: Text
    }

instance ToJSON Event where
    toJSON event =
        Aeson.object
            [ "id" .= event._id
            , "userId" .= event.userId
            , "name" .= event.name
            , "game" .= event.game
            ]

userIdField :: Text -> Field
userIdField = ("userId" :=) . Bson.String

nameField :: Text -> Field
nameField = ("name" :=) . Bson.String

gameField :: Text -> Field
gameField = ("game" :=) . Bson.String

decodeEvent :: Document -> Either String Event
decodeEvent doc =
    Event
        <$> Bson.lookup "_id" doc
        <*> Bson.lookup "userId" doc
        <*> Bson.lookup "name" doc
        <*> Bson.lookup "game" doc

collection :: Collection
collection = "events"