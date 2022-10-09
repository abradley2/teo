module Action.Auth.CheckAuthResponse (CheckAuthResponse (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Relude

newtype CheckAuthResponse = CheckAuthResponse {authorized :: Bool}

instance Aeson.ToJSON CheckAuthResponse where
    toJSON res =
        Aeson.object
            [ "authorized" .= res.authorized
            ]