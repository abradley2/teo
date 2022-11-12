port module Ports exposing (..)

import Json.Decode exposing (Value)


port linkClicked : (String -> msg) -> Sub msg


port storeData : ( String, Value ) -> Cmd msg


port requestData : String -> Cmd msg


port receiveData : (( String, Value ) -> msg) -> Sub msg


port replaceUrl : String -> Cmd msg


port pushUrl : String -> Cmd msg


port startRealm : String -> Cmd msg


port logError : String -> Cmd msg



-- Realm ports


port createEvent : Value -> Cmd msg


port createEventResponse : (Value -> msg) -> Sub msg


port requestEvents : () -> Cmd msg


port requestEventsResponse : (Value -> msg) -> Sub msg
