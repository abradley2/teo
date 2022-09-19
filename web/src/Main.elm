port module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Json.Decode as Decode exposing (Decoder, Error, Value)
import Page.Dashboard as Dashboard
import Page.Login as Login
import Result.Extra as Result
import Routes exposing (Route)
import State exposing (State(..))
import Url exposing (Url)


type Page
    = Login
    | Dashboard Dashboard.Model
    | NotFound


initPage : Route -> ( Page, Cmd Msg )
initPage route =
    case route of
        Routes.Login ->
            ( Login, Cmd.none )

        Routes.Dashboard ->
            Dashboard.init
                |> Tuple.mapFirst Dashboard
                |> Tuple.mapSecond (Cmd.map GotDashboardMsg)

        Routes.NotFound ->
            ( NotFound, Cmd.none )


type alias Flags =
    { url : String
    }


port linkClicked : (String -> msg) -> Sub msg


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map
        Flags
        (Decode.field "url" Decode.string)


type Msg
    = UrlChanged String
    | GotDashboardMsg Dashboard.Msg


type alias Model =
    { page : Page
    }


init : Value -> ( Result Error Model, Cmd Msg )
init flagsJson =
    let
        initResult =
            Decode.decodeValue flagsDecoder flagsJson
                |> Result.map
                    (\flags ->
                        let
                            ( page, cmd ) =
                                Routes.parseUrl flags.url
                                    |> initPage
                        in
                        ( { page = page
                          }
                        , cmd
                        )
                    )
    in
    case initResult of
        Ok ( model, cmd ) ->
            ( Ok model, cmd )

        Err err ->
            ( Err err, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GotDashboardMsg dashboardMsg, Dashboard page ) ->
            let
                ( nextPage, cmd ) =
                    Dashboard.update dashboardMsg page
                        |> Tuple.mapBoth Dashboard (Cmd.map GotDashboardMsg)
            in
            ( { model | page = nextPage }
            , cmd
            )

        ( GotDashboardMsg _, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged urlString, Dashboard page ) ->
            ( model, Dashboard.unload page |> Cmd.map GotDashboardMsg )


subscriptions : Model -> Sub Msg
subscriptions _ =
    linkClicked UrlChanged


view : Model -> Html Msg
view model =
    H.div
        []
        [ H.text "Hello world!" ]


errorView : Error -> Html Msg
errorView error =
    H.div
        []
        [ H.text <| Decode.errorToString error ]


main : Program Value (Result Error Model) Msg
main =
    Browser.element
        { init = init
        , update =
            \msg initModel ->
                case initModel of
                    Ok model ->
                        update msg model |> Tuple.mapFirst Ok

                    Err err ->
                        ( Err err, Cmd.none )
        , view = Result.map view >> Result.mapError errorView >> Result.merge
        , subscriptions =
            \initModel ->
                case initModel of
                    Ok model ->
                        subscriptions model

                    Err _ ->
                        Sub.none
        }
