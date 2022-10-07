module Page.Dashboard exposing (Effect(..), Model, Msg(..), init, perform, subscriptions, unload, update, view)

import AppAction exposing (AppAction)
import Html.Styled as H exposing (Html)
import Json.Decode exposing (Value)
import Ports
import Shared exposing (Shared)
import Translations.Dashboard


type Msg
    = NoOp
    | ReceivedData Value


type Effect
    = EffectNone


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectNone ->
            Cmd.none


type alias Model =
    {}


dataKey : String
dataKey =
    "dashboard-data"


init : ( Model, Maybe AppAction, Effect )
init =
    ( {}
    , Just (AppAction.RequestData dataKey)
    , EffectNone
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
unload _ =
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
