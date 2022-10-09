module Action.Auth.AuthorizeResponse (AuthorizeResponse (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Relude

data AuthorizeResponse = AuthorizeResponse {userId :: Text, authorized :: Bool}

instance Aeson.ToJSON AuthorizeResponse where
    toJSON res =
        Aeson.object
            [ "authorized" .= res.authorized
            , "userId" .= res.userId
            ]