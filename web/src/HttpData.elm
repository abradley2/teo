module HttpData exposing (HttpData(..))

import Http


type HttpData a
    = NotAsked
    | Loading (Maybe String)
    | Success a
    | Failure Http.Error
