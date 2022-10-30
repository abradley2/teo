module Page.Login exposing (Effect(..), LoginResponse, Model, Msg(..), encodeLoginRequest, init, loginResponseDecoder, loginUrl, perform, unload, update, view)

import AppAction exposing (AppAction, RealmJwt(..))
import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Http
import HttpData exposing (HttpData(..))
import I18Next exposing (Translations)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Shared exposing (ClientId, Shared)
import String.Verify
import Theme exposing (Theme)
import Translations.Login
import Verify
import Verify.Form exposing (FormValidator)
import View.Button as Button
import View.TextInput as TextInput


type alias Model =
    { loginRequest : HttpData ()
    , userId : String
    , userIdErrors : Maybe ( String, List String )
    }


type alias LoginRequest =
    { userId : String }


encodeLoginRequest : LoginRequest -> Value
encodeLoginRequest loginRequest =
    Encode.object
        [ ( "userId", Encode.string loginRequest.userId )
        ]


formValidator : FormValidator Model LoginRequest
formValidator =
    Verify.Form.validate LoginRequest
        |> Verify.Form.verify
            .userId
            (String.Verify.notBlank "user id may not be empty"
                |> Verify.compose (String.Verify.minLength 2 "user id must be at least 2 characters long")
                |> Verify.compose (String.Verify.maxLength 50 "user id must be at most 20 characters long")
                |> Verify.Form.liftValidator (\errors form -> { form | userIdErrors = Just errors })
            )


type Effect
    = EffectLogin ClientId (Maybe String) LoginRequest
    | EffectNone


loginUrl : String
loginUrl =
    "/api/login"


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectLogin clientId tracker loginRequest ->
            Http.request
                { method = "POST"
                , url = loginUrl
                , headers =
                    [ Shared.clientIdToHeader clientId
                    ]
                , tracker = tracker
                , body = Http.jsonBody <| encodeLoginRequest loginRequest
                , expect = Http.expectJson ReceivedLoginResponse loginResponseDecoder
                , timeout = Just 5000
                }

        EffectNone ->
            Cmd.none


type Msg
    = LoginClicked
    | UserIdChanged String
    | ReceivedLoginResponse (Result Http.Error LoginResponse)


type alias LoginResponse =
    { token : RealmJwt
    , userId : String
    }


loginResponseDecoder : Decoder LoginResponse
loginResponseDecoder =
    Decode.map2
        LoginResponse
        (Decode.field "token" (Decode.map RealmJwt Decode.string))
        (Decode.field "userId" Decode.string)


init : Model
init =
    { loginRequest = NotAsked
    , userId = ""
    , userIdErrors = Nothing
    }


update : Shared -> Msg -> Model -> ( Model, Maybe AppAction, Effect )
update shared msg model =
    case msg of
        UserIdChanged userId ->
            ( { model | userId = userId }
            , Nothing
            , EffectNone
            )

        LoginClicked ->
            case Verify.Form.run formValidator model of
                Ok loginRequest ->
                    let
                        tracker : Maybe String
                        tracker =
                            Just "login request"
                    in
                    ( { model | loginRequest = Loading tracker }
                    , Nothing
                    , EffectLogin shared.clientId tracker loginRequest
                    )

                Err modelWithErrors ->
                    ( modelWithErrors
                    , Nothing
                    , EffectNone
                    )

        ReceivedLoginResponse (Err err) ->
            ( { model | loginRequest = Failure err }
            , Just <|
                AppAction.ShowNotification
                    AppAction.NotificationError
                    (Translations.Login.loginFailedMessage shared.language)
            , EffectNone
            )

        ReceivedLoginResponse (Ok res) ->
            ( { model | loginRequest = Success () }
            , Just <|
                AppAction.Batch
                    [ AppAction.ReplaceUrl "/"
                    , AppAction.StartRealm res.token
                    ]
            , EffectNone
            )


unload : Model -> Maybe AppAction
unload model =
    Just <| AppAction.cancelRequest model.loginRequest


userIdInput : Shared -> Model -> ( String, Html Msg )
userIdInput shared model =
    let
        id =
            "login-user-text-input"
    in
    TextInput.config
        { id = id
        , label = Translations.Login.userIdInputLabel shared.language
        , theme = shared.theme
        }
        |> TextInput.withValue model.userId
        |> TextInput.withOnInput (Just UserIdChanged)
        |> TextInput.withErrorMessage
            (model.loginRequest
                |> HttpData.toFailure
                |> Maybe.map (Translations.Login.userIdInputGenericError shared.language |> always)
            )
        |> TextInput.view
        |> Tuple.pair id


view : Shared -> Model -> Html Msg
view shared model =
    let
        language : List Translations
        language =
            shared.language

        theme : Theme
        theme =
            shared.theme
    in
    H.div
        [ A.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.flexDirection Css.column
            , Css.height (Css.pct 60)
            , Css.width (Css.pct 100)
            ]
        ]
        [ H.div
            []
            [ Tuple.second (userIdInput shared model)
            ]
        , H.div
            [ A.css
                [ Css.marginTop (Css.rem 1)
                ]
            ]
            [ Button.config
                { label = Translations.Login.buttonPrompt language
                , theme = theme
                }
                |> Button.withOnClick LoginClicked
                |> Button.view
            ]
        ]
