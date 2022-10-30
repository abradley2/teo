module AppAction exposing (AppAction(..), Notification(..), RealmJwt(..), cancelRequest)

import HttpData exposing (HttpData(..))
import Json.Encode exposing (Value)


type AppAction
    = None
    | ReplaceUrl String
    | Logout
    | CancelRequest String
    | RequestData String
    | StoreData String Value
    | ShowNotification Notification String
    | StartRealm RealmJwt
    | Batch (List AppAction)


cancelRequest : HttpData a -> AppAction
cancelRequest data =
    case data of
        Loading (Just tracker) ->
            CancelRequest tracker

        _ ->
            None


type Notification
    = NotificationError


type RealmJwt
    = RealmJwt String
