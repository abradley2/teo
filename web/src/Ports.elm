port module Ports exposing (..)

import Json.Decode exposing (Value)


port linkClicked : (String -> msg) -> Sub msg


port storeData : { key : String, value : Value } -> Cmd msg


port requestData : String -> Cmd msg


port receiveData : (( String, Value ) -> msg) -> Sub msg


port replaceUrl : String -> Cmd msg


port pushUrl : String -> Cmd msg


port realmJwt : String -> Cmd msg


port logError : String -> Cmd msg



-- Realm ports


port createEvent : Value -> Cmd msg


port createEventResponse : (Value -> msg) -> Sub msg


port requestHostingEvents : Value -> Cmd msg


port requestHostingEventsResponse : (Value -> msg) -> Sub msg


port requestParticipatingEvents : Value -> Cmd msg


port requestParticipatingEventsResponse : (Value -> msg) -> Sub msg
