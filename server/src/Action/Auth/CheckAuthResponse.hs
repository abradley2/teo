module Action.Auth.CheckAuthResponse (CheckAuthResponse (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Relude

data CheckAuthResponse = CheckAuthResponse {userId :: Maybe Text, token :: Maybe Text}

instance Aeson.ToJSON CheckAuthResponse where
    toJSON res =
        Aeson.object
            [ "userId" .= res.userId
            , "token" .= res.token
            ]