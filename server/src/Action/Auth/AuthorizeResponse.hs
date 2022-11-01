module Action.Auth.AuthorizeResponse (AuthorizeResponse (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Relude

data AuthorizeResponse = AuthorizeResponse {userId :: Text, token :: Text}

instance Aeson.ToJSON AuthorizeResponse where
    toJSON res =
        Aeson.object
            [ "userId" .= res.userId
            , "token" .= res.token
            ]