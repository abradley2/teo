module HttpData exposing (HttpData(..), httpResponseSub, toFailure)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Result.Extra as ResultX


type HttpData a
    = NotAsked
    | Loading (Maybe String)
    | Success a
    | Failure Http.Error


httpDataDecoder : Decoder a -> Decoder (Result Http.Error a)
httpDataDecoder responseDecoder =
    Decode.value
        |> Decode.andThen
            (\value ->
                case Decode.decodeValue (Decode.field "status" Decode.string) value of
                    Ok "Success" ->
                        Decode.decodeValue (Decode.field "response" responseDecoder) value
                            |> Result.mapError (Decode.errorToString >> Http.BadBody)
                            |> Decode.succeed

                    Ok "Failure" ->
                        Decode.decodeValue (Decode.field "error" Decode.string) value
                            |> Result.map Http.BadBody
                            |> Result.mapError (Decode.errorToString >> Http.BadBody)
                            |> ResultX.merge
                            |> Err
                            |> Decode.succeed

                    Ok status ->
                        Decode.fail <| "Invalid status: " ++ status ++ " , must be Success or Failure"

                    Err err ->
                        Decode.fail (Decode.errorToString err)
            )


toFailure : HttpData a -> Maybe Http.Error
toFailure data =
    case data of
        Failure error ->
            Just error

        _ ->
            Nothing


httpResponseSub : (Result Http.Error a -> msg) -> Decoder a -> Value -> msg
httpResponseSub toMsg decoder =
    Decode.decodeValue (httpDataDecoder decoder)
        >> Result.mapError (Decode.errorToString >> Http.BadBody)
        >> ResultX.join
        >> toMsg
