module SimulatedEffect.Page.Login exposing (..)

import Page.Login as Login
import ProgramTest exposing (SimulatedEffect)
import Shared exposing (ClientId(..))
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Http as SimulatedHttp


perform : Login.Effect -> SimulatedEffect Login.Msg
perform effect =
    case effect of
        Login.EffectNone ->
            SimulatedCmd.none

        Login.EffectLogin (ClientId clientId) tracker loginRequest ->
            SimulatedHttp.request
                { method = "POST"
                , url = Login.loginUrl
                , headers =
                    [ SimulatedHttp.header "X-Client-Id" clientId
                    ]
                , tracker = tracker
                , body = SimulatedHttp.jsonBody <| Login.encodeLoginRequest loginRequest
                , expect = SimulatedHttp.expectJson Login.ReceivedLoginResponse Login.loginResponseDecoder
                , timeout = Just 5000
                }
