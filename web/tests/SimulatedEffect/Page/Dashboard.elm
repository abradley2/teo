module SimulatedEffect.Page.Dashboard exposing (..)

import HttpData exposing (RequestTag(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Dashboard as Dashboard
import ProgramTest exposing (SimulatedEffect)
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Http as SimulatedHttp
import Url.Builder
import User exposing (UserId(..))


perform : Dashboard.Effect -> SimulatedEffect Dashboard.Msg
perform effect =
    case effect of
        Dashboard.EffectRequestEvents (RequestTag tag) (UserId userId) ->
            SimulatedHttp.request
                { method = "GET"
                , timeout = Nothing
                , expect =
                    SimulatedHttp.expectJson
                        Dashboard.ReceivedEventsResponse
                        (Decode.list Dashboard.eventDecoder)
                , body =
                    SimulatedHttp.jsonBody <|
                        Encode.object
                            [ ( "userId", Encode.string userId )
                            , ( "tag", Encode.string tag )
                            ]
                , url = Url.Builder.absolute [ "/api" ] [ Url.Builder.string "tag" tag ]
                , tracker = Nothing
                , headers = []
                }

        Dashboard.EffectRequestParticipatingEvents (RequestTag tag) (UserId userId) ->
            SimulatedHttp.request
                { method = "GET"
                , timeout = Nothing
                , expect =
                    SimulatedHttp.expectJson
                        Dashboard.ReceivedParticipatingEventsResponse
                        (Decode.list Dashboard.eventDecoder)
                , body =
                    SimulatedHttp.jsonBody <|
                        Encode.object
                            [ ( "userId", Encode.string userId )
                            , ( "tag", Encode.string tag )
                            ]
                , url = Url.Builder.absolute [ "/api" ] [ Url.Builder.string "tag" tag ]
                , tracker = Nothing
                , headers = []
                }

        Dashboard.EffectBatch effs ->
            SimulatedCmd.batch (List.map perform effs)

        Dashboard.EffectNone ->
            SimulatedCmd.none
