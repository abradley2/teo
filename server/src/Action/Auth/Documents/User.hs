module Action.Auth.Documents.User (User (..), encodeUser, decodeUser, collection, userIdField) where

import Data.Bson (Field)
import Data.Bson qualified as Bson
import Database.MongoDB (Collection, (=:))
import Relude

collection :: Collection
collection = "users"

data User = User
    { _id :: Maybe Bson.ObjectId
    , userId :: Text
    }

userIdField :: Text -> Field
userIdField = ("userId" =:) . Bson.String

encodeUser :: User -> Bson.Document
encodeUser user =
    [ "userId" =: Bson.String user.userId
    ]
        <> case user._id of
            Just oid -> ["_id" =: oid]
            Nothing -> []

decodeUser :: Bson.Document -> Either Text User
decodeUser doc =
    User
        <$> Bson.lookup "_id" doc
        <*> Bson.lookup "userId" doc