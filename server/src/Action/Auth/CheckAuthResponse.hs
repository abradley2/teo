module Action.Auth.CheckAuthResponse (CheckAuthResponse (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Relude

data CheckAuthResponse = CheckAuthResponse {authorized :: Bool, token :: Maybe Text}

instance Aeson.ToJSON CheckAuthResponse where
    toJSON res =
        Aeson.object
            [ "authorized" .= res.authorized
            , "token" .= res.token
            ]