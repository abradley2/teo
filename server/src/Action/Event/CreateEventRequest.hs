module Action.Event.CreateEventRequest (CreateEventRequest (..)) where

import Data.Aeson (FromJSON, (.:))
import Data.Aeson qualified as Aeson
import Relude

data CreateEventRequest = CreateEventRequest
    { name :: Text
    , game :: Text
    }

instance FromJSON CreateEventRequest where
    parseJSON = Aeson.withObject "CreateEventRequest" $ \obj -> do
        CreateEventRequest
            <$> (obj .: "name")
            <*> (obj .: "game")