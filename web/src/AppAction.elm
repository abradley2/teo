module AppAction exposing (..)

import Json.Encode exposing (Value)


type AppAction
    = PushUrl String
    | ReplaceUrl String
    | Logout
    | CancelRequest String
    | StoreData String Value
    | LoadData String Value
