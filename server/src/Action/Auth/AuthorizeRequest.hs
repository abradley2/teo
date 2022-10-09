module Action.Auth.AuthorizeRequest (AuthorizeRequest (..)) where

import Relude

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson

newtype AuthorizeRequest = AuthorizeRequest
    { userId :: Text
    }

instance Aeson.FromJSON AuthorizeRequest where
    parseJSON = Aeson.withObject "AuthorizeRequest" $ \obj -> do
        userId <- obj .: "userId"
        pure $ AuthorizeRequest userId