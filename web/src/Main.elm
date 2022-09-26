port module Main exposing (..)

import AppAction exposing (AppAction)
import Browser
import Html.Styled as H exposing (Html)
import Http
import HttpData exposing (HttpData)
import I18Next exposing (Translations)
import Json.Decode as Decode exposing (Decoder, Error, Value)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Page.Dashboard as Dashboard
import Page.Login as Login
import Result.Extra as Result
import Routes exposing (Route)
import Shared exposing (LanguageId, Shared)
import Tuple3
import User exposing (User)


type Effect
    = EffectNone
    | EffectDashboard Dashboard.Effect
    | EffectCheckAuth
    | EffectBatch (List Effect)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectNone ->
            Cmd.none

        EffectCheckAuth ->
            Http.request
                { method = "GET"
                , url = "/api/check-auth"
                , body = Http.emptyBody
                , tracker = Nothing
                , expect = Http.expectJson CheckedUserAuthorization (Decode.field "authorized" Decode.bool)
                , headers = []
                , timeout = Nothing
                }

        EffectDashboard subEffect ->
            Dashboard.perform subEffect |> Cmd.map GotDashboardMsg

        EffectBatch effects ->
            Cmd.batch (List.map perform effects)


type Page
    = Login
    | Dashboard Dashboard.Model
    | NotFound


type alias Flags =
    { url : String
    , languages : Nonempty ( LanguageId, Translations )
    }


port linkClicked : (String -> msg) -> Sub msg


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map2
        Flags
        (Decode.field "url" Decode.string)
        (Decode.field "languages"
            (Decode.list
                (Decode.map2
                    Tuple.pair
                    (Decode.field "0" Shared.translationIdDecoder)
                    (Decode.field "1" I18Next.translationsDecoder)
                )
            )
            |> Decode.andThen
                (Nonempty.fromList
                    >> Maybe.map Decode.succeed
                    >> Maybe.withDefault (Decode.fail "No translations")
                )
        )


type Msg
    = RouteChanged Route
    | CheckedUserAuthorization (Result Http.Error Bool)
    | GotDashboardMsg Dashboard.Msg


type alias Model =
    { page : Page
    , user : HttpData User
    , shared : Shared
    }


init : Value -> ( Result Error Model, Effect )
init flagsJson =
    let
        initResult =
            Decode.decodeValue flagsDecoder flagsJson
                |> Result.map
                    (\flags ->
                        ( { page = NotFound
                          , shared = Shared.init flags.languages
                          , user = HttpData.Loading Nothing
                          }
                        , EffectCheckAuth
                        )
                            |> withRoute (Routes.parseUrl flags.url)
                    )
    in
    case initResult of
        Ok ( model, effect ) ->
            ( Ok model, effect )

        Err err ->
            ( Err err, EffectNone )


withRoute : Route -> ( Model, Effect ) -> ( Model, Effect )
withRoute route ( model, effect ) =
    let
        appendEffect newEffect =
            EffectBatch [ effect, newEffect ]
    in
    case route of
        Routes.NotFound ->
            ( { model | page = NotFound }, effect )

        Routes.Login ->
            ( { model | page = Login }, effect )

        Routes.Dashboard ->
            let
                ( page, appAction, pageEffect ) =
                    Dashboard.init
                        |> Tuple3.mapFirst Dashboard
                        |> Tuple3.mapThird EffectDashboard
            in
            ( { model | page = page }, appendEffect pageEffect )
                |> withAppAction appAction


withAppAction : Maybe AppAction -> ( Model, Effect ) -> ( Model, Effect )
withAppAction action ( model, effect ) =
    let
        appendEffect newEffect =
            EffectBatch [ effect, newEffect ]
    in
    case action of
        Just _ ->
            ( model, effect )

        Nothing ->
            ( model, effect )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case ( msg, model.page ) of
        ( GotDashboardMsg dashboardMsg, Dashboard page ) ->
            let
                ( nextPage, appAction, effect ) =
                    Dashboard.update dashboardMsg page
                        |> Tuple3.mapFirst Dashboard
                        |> Tuple3.mapThird EffectDashboard
            in
            withAppAction appAction ( { model | page = nextPage }, effect )

        ( CheckedUserAuthorization (Ok True), _ ) ->
            ( model, EffectNone )

        ( CheckedUserAuthorization (Ok False), _ ) ->
            ( model, EffectNone )

        ( CheckedUserAuthorization (Err _), _ ) ->
            ( model, EffectNone )

        ( GotDashboardMsg _, _ ) ->
            ( model, EffectNone )

        ( RouteChanged route, Dashboard page ) ->
            withAppAction (Dashboard.unload page) ( model, EffectNone )
                |> withRoute route

        ( RouteChanged route, Login ) ->
            ( model, EffectNone )
                |> withRoute route

        ( RouteChanged route, NotFound ) ->
            ( model, EffectNone )
                |> withRoute route


subscriptions : Model -> Sub Msg
subscriptions _ =
    linkClicked (Routes.parseUrl >> RouteChanged)


view : Model -> Html Msg
view model =
    case model.page of
        Login ->
            Login.view

        Dashboard page ->
            Dashboard.view model.shared page
                |> H.map GotDashboardMsg

        NotFound ->
            H.text "Page not found"


errorView : Error -> Html Msg
errorView error =
    H.div
        []
        [ H.text <| Decode.errorToString error ]


main : Program Value (Result Error Model) Msg
main =
    Browser.element
        { init = init >> Tuple.mapSecond perform
        , update =
            \msg initModel ->
                case initModel of
                    Ok model ->
                        update msg model
                            |> Tuple.mapFirst Ok
                            |> Tuple.mapSecond perform

                    Err err ->
                        ( Err err, Cmd.none )
        , view = Result.map view >> Result.mapError errorView >> Result.merge >> H.toUnstyled
        , subscriptions =
            \initModel ->
                case initModel of
                    Ok model ->
                        subscriptions model

                    Err _ ->
                        Sub.none
        }
