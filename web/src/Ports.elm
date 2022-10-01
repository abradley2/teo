port module Ports exposing (linkClicked, pushUrl, receiveData, replaceUrl, requestData, storeData)

import Json.Decode exposing (Value)


port linkClicked : (String -> msg) -> Sub msg


port storeData : ( String, Value ) -> Cmd msg


port requestData : String -> Cmd msg


port receiveData : (( String, Value ) -> msg) -> Sub msg


port replaceUrl : String -> Cmd msg


port pushUrl : String -> Cmd msg
