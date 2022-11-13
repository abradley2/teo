module HttpData exposing (HttpData(..), httpErrorToString, httpResponseSub, toFailure)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Result.Extra as ResultX


type HttpData a
    = NotAsked
    | Loading (Maybe String)
    | Success a
    | Failure Http.Error


httpDataDecoder : String -> Decoder a -> Decoder (Maybe (Result Http.Error a))
httpDataDecoder tag responseDecoder =
    Decode.value
        |> Decode.andThen
            (\value ->
                if Decode.decodeValue (Decode.field "tag" Decode.string) value == Ok tag then
                    case
                        Decode.decodeValue (Decode.field "status" Decode.string) value
                    of
                        Ok "Success" ->
                            Decode.decodeValue (Decode.field "response" responseDecoder) value
                                |> Result.mapError (Decode.errorToString >> Http.BadBody)
                                |> Just
                                |> Decode.succeed

                        Ok "Failure" ->
                            Decode.decodeValue (Decode.field "error" Decode.string) value
                                |> Result.map Http.BadBody
                                |> Result.mapError (Decode.errorToString >> Http.BadBody)
                                |> ResultX.merge
                                |> Err
                                |> Just
                                |> Decode.succeed

                        Ok status ->
                            Decode.fail <| "Invalid status: " ++ status ++ " , must be Success or Failure"

                        Err err ->
                            Decode.fail (Decode.errorToString err)

                else
                    Decode.succeed Nothing
            )


toFailure : HttpData a -> Maybe Http.Error
toFailure data =
    case data of
        Failure error ->
            Just error

        _ ->
            Nothing


httpResponseSub : String -> (Maybe (Result Http.Error a) -> msg) -> Decoder a -> Value -> msg
httpResponseSub tag toMsg decoder =
    Decode.decodeValue (httpDataDecoder tag decoder)
        >> Result.mapError (\err -> toMsg (Just << Err <| Http.BadBody (Decode.errorToString err)))
        >> Result.map toMsg
        >> ResultX.merge


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadBody msg ->
            "Bad body: " ++ msg

        Http.BadStatus msg ->
            "Unexpected status code: " ++ String.fromInt msg

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadUrl msg ->
            "Bad url: " ++ msg
