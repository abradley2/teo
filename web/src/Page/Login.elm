module Page.Login exposing (Effect(..), LoginResponse, Model, Msg(..), init, loginResponseDecoder, loginUrl, perform, subscriptions, unload, update, view)

import AppAction exposing (AppAction)
import Html.Styled as H exposing (Html)
import Html.Styled.Events as E
import Http
import HttpData exposing (HttpData(..))
import Json.Decode as Decode exposing (Decoder, Value)
import Ports
import Shared exposing (ClientId, Shared)
import Translations.Login


type alias Model =
    { loginRequest : HttpData ()
    }


type Effect
    = EffectLogin ClientId (Maybe String)
    | EffectNone


loginUrl : String
loginUrl =
    "/api/login"


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectLogin clientId tracker ->
            Http.request
                { method = "POST"
                , url = loginUrl
                , headers =
                    [ Shared.clientIdToHeader clientId
                    ]
                , tracker = tracker
                , body = Http.emptyBody
                , expect = Http.expectJson ReceivedLoginResponse loginResponseDecoder
                , timeout = Just 5000
                }

        EffectNone ->
            Cmd.none


type Msg
    = NoOp
    | ReceivedData Value
    | LoginClicked
    | ReceivedLoginResponse (Result Http.Error LoginResponse)


type alias LoginResponse =
    { authorized : Bool }


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    Decode.map LoginResponse
        (Decode.field "authorized" Decode.bool)


init : Model
init =
    { loginRequest = NotAsked
    }


update : Shared -> Msg -> Model -> ( Model, Maybe AppAction, Effect )
update shared msg model =
    case msg of
        NoOp ->
            ( model, Nothing, EffectNone )

        ReceivedData _ ->
            ( model, Nothing, EffectNone )

        LoginClicked ->
            let
                tracker : Maybe String
                tracker =
                    Just "login request"
            in
            ( { model | loginRequest = Loading tracker }
            , Nothing
            , EffectLogin shared.clientId tracker
            )

        ReceivedLoginResponse (Err err) ->
            ( { model | loginRequest = Failure err }
            , Just <|
                AppAction.ShowNotification
                    AppAction.NotificationError
                    "Login failed"
            , EffectNone
            )

        ReceivedLoginResponse (Ok _) ->
            ( { model | loginRequest = Success () }
            , Just <| AppAction.ReplaceUrl "/"
            , EffectNone
            )


unload : Model -> Maybe AppAction
unload model =
    Just <| AppAction.cancelRequest model.loginRequest


dataKey : String
dataKey =
    "login-data"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveData
        (\( key, value ) ->
            if key == dataKey then
                ReceivedData value

            else
                NoOp
        )


view : Shared -> Model -> Html Msg
view shared _ =
    H.div
        []
        [ H.button
            [ E.onClick LoginClicked ]
            [ H.text <|
                Translations.Login.buttonPrompt shared.language
            ]
        ]
