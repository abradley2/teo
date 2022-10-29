module Action.Auth.Documents.User (User (..), encodeUser, decodeUser, collection, userIdField) where

import Data.Bson (Field)
import Data.Bson qualified as Bson
import Database.MongoDB (Collection, (=:))
import Relude

collection :: Collection
collection = "users"

newtype User = User {userId :: Text}

userIdField :: Text -> Field
userIdField = ("userId" =:) . Bson.String

encodeUser :: User -> Bson.Document
encodeUser user =
    [ "userId" =: Bson.String user.userId
    ]

decodeUser :: Bson.Document -> Either Text User
decodeUser doc =
    User
        <$> Bson.lookup "userId" doc