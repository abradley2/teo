module HttpData exposing (HttpData(..), toFailure)

import Http


type HttpData a
    = NotAsked
    | Loading (Maybe String)
    | Success a
    | Failure Http.Error


toFailure : HttpData a -> Maybe Http.Error
toFailure data =
    case data of
        Failure error ->
            Just error

        _ ->
            Nothing
