module Page.Dashboard exposing (..)

import AppAction exposing (AppAction)
import Html as H exposing (Html)
import Shared exposing (Shared)
import Translations.Dashboard


type Msg
    = NoOp


type Effect
    = EffectNone


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectNone ->
            Cmd.none


type alias Model =
    {}


init : ( Model, Maybe AppAction, Effect )
init =
    ( {}, Nothing, EffectNone )


unload : Model -> Maybe AppAction
unload model =
    Nothing


update : Msg -> Model -> ( Model, Maybe AppAction, Effect )
update msg model =
    ( model, Nothing, EffectNone )


view : Shared -> Model -> Html Msg
view shared model =
    H.div
        []
        [ H.text <|
            Translations.Dashboard.greeting shared.language "Tony"
        ]
