module Page.Dashboard exposing (Effect(..), Model, Msg(..), init, perform, subscriptions, unload, update, view)

import AppAction exposing (AppAction)
import Html.Styled as H exposing (Html)
import Http
import HttpData exposing (HttpData(..))
import Json.Decode as Decode exposing (Decoder, Value)
import Ports
import Shared exposing (ClientId, Shared)
import Translations.Dashboard


type alias Event =
    { id : String
    , userId : String
    , name : String
    , game : String
    }


eventDecoder : Decoder Event
eventDecoder =
    Decode.map4
        Event
        (Decode.field "_id" Decode.string)
        (Decode.field "userId" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "game" Decode.string)


type Msg
    = NoOp
    | ReceivedEventsResponse (Result Http.Error (List Event))
    | ReceivedData Value


type Effect
    = EffectNone
    | EffectRequestEvents ClientId String


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectRequestEvents clientId tracker ->
            Http.request
                { method = "GET"
                , url = "/api/events"
                , headers =
                    [ Shared.clientIdToHeader clientId
                    ]
                , timeout = Just 5000
                , tracker = Just tracker
                , expect = Http.expectJson ReceivedEventsResponse (Decode.list eventDecoder)
                , body = Http.emptyBody
                }

        EffectNone ->
            Cmd.none


type alias Model =
    { events : HttpData (List Event)
    }


dataKey : String
dataKey =
    "dashboard-data"


init : Shared -> ( Model, Maybe AppAction, Effect )
init shared =
    let
        getEventsTracker : String
        getEventsTracker =
            "get-events-request"
    in
    ( { events = Loading (Just getEventsTracker)
      }
    , Just (AppAction.RequestData dataKey)
    , EffectRequestEvents shared.clientId getEventsTracker
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveData
        (\( key, value ) ->
            if key == dataKey then
                ReceivedData value

            else
                NoOp
        )


unload : Model -> Maybe AppAction
unload model =
    case model.events of
        Loading (Just tracker) ->
            Just (AppAction.CancelRequest tracker)

        _ ->
            Nothing


update : Shared -> Msg -> Model -> ( Model, Maybe AppAction, Effect )
update shared msg model =
    ( model, Nothing, EffectNone )


view : Shared -> Model -> Html Msg
view shared model =
    H.div
        []
        [ H.text <|
            Translations.Dashboard.greeting shared.language { name = "Tony" }
        ]
