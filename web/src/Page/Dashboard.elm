module Page.Dashboard exposing (Effect(..), Model, Msg(..), init, perform, subscriptions, unload, update, view)

import AppAction exposing (AppAction)
import Html.Styled as H exposing (Html)
import Http
import HttpData exposing (HttpData(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import Shared exposing (Shared)
import Translations.Dashboard
import User exposing (User)


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
    | EffectRequestEvents
    | EffectCreateEvent Value
    | EffectBatch (List Effect)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectRequestEvents ->
            Ports.requestEvents ()

        EffectCreateEvent value ->
            Ports.createEvent value

        EffectBatch effects ->
            List.foldr
                (\effect_ cmd -> Cmd.batch [ perform effect_, cmd ])
                Cmd.none
                effects

        EffectNone ->
            Cmd.none


type alias Model =
    { events : HttpData (List Event)
    }


dataKey : String
dataKey =
    "dashboard-data"


init : User -> Shared -> ( Model, Maybe AppAction, Effect )
init user shared =
    let
        getEventsTracker : String
        getEventsTracker =
            "get-events-request"
    in
    ( { events = Loading (Just getEventsTracker)
      }
    , Just (AppAction.RequestData dataKey)
    , EffectBatch
        [ EffectRequestEvents
        , EffectCreateEvent
            (Encode.object
                [ ( "userId", Encode.string (User.userId user) )
                , ( "name", Encode.string "Test tournament" )
                , ( "game", Encode.string "Warhammer" )
                ]
            )
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveData
            (\( key, value ) ->
                if key == dataKey then
                    ReceivedData value

                else
                    NoOp
            )
        , HttpData.httpResponseSub ReceivedEventsResponse (Decode.list eventDecoder)
            |> Ports.requestEventsResponse
        ]


unload : Model -> Maybe AppAction
unload model =
    case model.events of
        Loading (Just tracker) ->
            Just (AppAction.CancelRequest tracker)

        _ ->
            Nothing


update : User -> Shared -> Msg -> Model -> ( Model, Maybe AppAction, Effect )
update user shared msg model =
    ( model, Nothing, EffectNone )


view : Shared -> Model -> Html Msg
view shared model =
    H.div
        []
        [ H.text <|
            Translations.Dashboard.greeting shared.language { name = "Tony" }
        ]
