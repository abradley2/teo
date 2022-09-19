module Page.Dashboard exposing (..)

import Html as H exposing (Html)


type Msg
    = NoOp


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


unload : Model -> Cmd Msg
unload model =
    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    H.div
        []
        [ H.text "Login Page" ]
