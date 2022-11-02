module Main exposing
    ( Effect(..)
    , Flags
    , Model
    , Msg(..)
    , Page(..)
    , checkAuthResponseDecoder
    , checkAuthUrl
    , initWithFlags
    , main
    , update
    , view
    )

import AppAction exposing (AppAction, Notification, RealmJwt(..))
import Browser
import Html.Styled as H exposing (Html)
import Http
import HttpData exposing (HttpData)
import I18Next exposing (Translations)
import Json.Decode as Decode exposing (Decoder, Error, Value)
import Language
import Layout
import List.Nonempty exposing (Nonempty(..))
import Page.Dashboard as Dashboard
import Page.Login as Login
import Ports
import Result.Extra as Result
import Routes exposing (Route)
import Shared exposing (ClientId(..), LanguageId, Shared)
import Translations
import Tuple3
import User exposing (User, UserId(..))


type Effect
    = EffectNone
    | EffectDashboard Dashboard.Effect
    | EffectLogin Login.Effect
    | EffectCheckAuth ClientId String
    | EffectStoreData String Value
    | EffectRequestData String
    | EffectCancelRequest String
    | EffectPushUrl String
    | EffectReplaceUrl String
    | EffectStartRealm RealmJwt
    | EffectBatch (List Effect)


checkAuthUrl : String
checkAuthUrl =
    "/api/check-auth"


checkAuthResponseDecoder : Decoder (Maybe ( UserId, RealmJwt ))
checkAuthResponseDecoder =
    Decode.map2
        Tuple.pair
        (Decode.field "token" (Decode.nullable (Decode.map RealmJwt Decode.string)))
        (Decode.field "userId" (Decode.nullable (Decode.map UserId Decode.string)))
        |> Decode.andThen
            (\( token, userId ) ->
                Maybe.map2
                    (\token_ userId_ -> Just ( userId_, token_ ) |> Decode.succeed)
                    token
                    userId
                    |> Maybe.withDefault (Decode.succeed Nothing)
            )


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectNone ->
            Cmd.none

        EffectPushUrl url ->
            Ports.pushUrl url

        EffectReplaceUrl url ->
            Ports.replaceUrl url

        EffectCancelRequest tracker ->
            Http.cancel tracker

        EffectStoreData k v ->
            Ports.storeData ( k, v )

        EffectRequestData k ->
            Ports.requestData k

        EffectStartRealm (RealmJwt jwt) ->
            Ports.startRealm jwt

        EffectCheckAuth clientId redirectUrl ->
            Http.request
                { method = "GET"
                , url = checkAuthUrl
                , body = Http.emptyBody
                , tracker = Nothing
                , expect = Http.expectJson (CheckedUserAuthorization redirectUrl) checkAuthResponseDecoder
                , headers =
                    [ Shared.clientIdToHeader clientId
                    ]
                , timeout = Just 5000
                }

        EffectDashboard subEffect ->
            Dashboard.perform subEffect |> Cmd.map GotDashboardMsg

        EffectLogin subEffect ->
            Login.perform subEffect |> Cmd.map GotLoginMsg

        EffectBatch effects ->
            Cmd.batch (List.map perform effects)


type Page
    = CheckAuth
    | Login Login.Model
    | Dashboard User Dashboard.Model
    | NotFound


type alias Flags =
    { url : String
    , languages : Nonempty ( LanguageId, Translations )
    , clientId : ClientId
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map3
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
            |> Decode.map
                (Nonempty ( Shared.EN, Language.defaultLanguage ))
        )
        (Decode.field "clientId" Decode.string
            |> Decode.map ClientId
        )


type Msg
    = NoOp
    | RouteChanged Route
    | CheckedUserAuthorization String (Result Http.Error (Maybe ( UserId, RealmJwt )))
    | GotLayoutMsg Layout.Msg
    | GotDashboardMsg Dashboard.Msg
    | GotLoginMsg Login.Msg


type alias Model =
    { page : Page
    , layout : Layout.Model
    , user : HttpData (Maybe User)
    , notification : Maybe ( Notification, String )
    , shared : Shared
    }


initWithFlags : Flags -> ( Model, Effect )
initWithFlags flags =
    ( { page = CheckAuth
      , shared = Shared.init flags.clientId flags.languages
      , notification = Nothing
      , user = HttpData.Loading Nothing
      , layout = Layout.init
      }
    , EffectCheckAuth flags.clientId flags.url
    )


init : Value -> ( Result Error Model, Effect )
init flagsJson =
    let
        initResult : Result Error ( Model, Effect )
        initResult =
            Decode.decodeValue flagsDecoder flagsJson
                |> Result.map initWithFlags
    in
    case initResult of
        Ok ( model, effect ) ->
            ( Ok model, effect )

        Err err ->
            ( Err err, EffectNone )


withRoute : Route -> ( Model, Effect ) -> ( Model, Effect )
withRoute route ( model, effect ) =
    let
        appendEffect : Effect -> Effect
        appendEffect newEffect =
            EffectBatch [ effect, newEffect ]

        ( nextModel, nextEffect ) =
            case ( route, model.user ) of
                ( Routes.NotFound, _ ) ->
                    ( { model | page = NotFound }, Nothing )

                ( Routes.Login, _ ) ->
                    ( { model | page = Login Login.init }, Nothing )

                ( Routes.Dashboard, HttpData.Success (Just user) ) ->
                    let
                        ( page, appAction, pageEffect ) =
                            Dashboard.init user model.shared
                                |> Tuple3.mapFirst (Dashboard user)
                                |> Tuple3.mapThird EffectDashboard
                    in
                    ( { model | page = page }, pageEffect )
                        |> withAppAction appAction
                        |> Tuple.mapSecond Just

                ( Routes.Dashboard, _ ) ->
                    ( { model | page = NotFound }, Nothing )
    in
    ( nextModel, Maybe.map appendEffect nextEffect |> Maybe.withDefault effect )


withAppAction : Maybe AppAction -> ( Model, Effect ) -> ( Model, Effect )
withAppAction action ( model, effect ) =
    let
        appendEffect : Effect -> Effect
        appendEffect newEffect =
            EffectBatch [ effect, newEffect ]

        ( nextModel, nextEffect ) =
            case action of
                Just (AppAction.ShowNotification notificationType message) ->
                    ( { model
                        | notification = Just ( notificationType, message )
                      }
                    , Nothing
                    )

                Just (AppAction.ReplaceUrl url) ->
                    ( model, Just <| EffectReplaceUrl url )

                Just (AppAction.CancelRequest tracker) ->
                    ( model, Just (EffectCancelRequest tracker) )

                Just AppAction.Logout ->
                    ( model, Just <| EffectReplaceUrl "/app/login" )

                Just (AppAction.StoreData k v) ->
                    ( model, Just (EffectStoreData k v) )

                Just (AppAction.RequestData k) ->
                    ( model, Just (EffectRequestData k) )

                Just (AppAction.StartRealm token) ->
                    ( model, Just <| EffectStartRealm token )

                Just (AppAction.Batch actions) ->
                    List.foldr
                        (\next acc -> withAppAction (Just next) acc)
                        ( model, EffectNone )
                        actions
                        |> Tuple.mapSecond Just

                Just AppAction.None ->
                    ( model, Nothing )

                Nothing ->
                    ( model, Nothing )
    in
    ( nextModel, Maybe.map appendEffect nextEffect |> Maybe.withDefault effect )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case ( msg, model.page ) of
        ( NoOp, _ ) ->
            ( model, EffectNone )

        ( GotLayoutMsg layoutMsg, _ ) ->
            ( { model | layout = Layout.update layoutMsg model.layout }, EffectNone )

        ( GotDashboardMsg dashboardMsg, Dashboard user page ) ->
            let
                ( nextPage, appAction, effect ) =
                    Dashboard.update user model.shared dashboardMsg page
                        |> Tuple3.mapFirst (Dashboard user)
                        |> Tuple3.mapThird EffectDashboard
            in
            withAppAction appAction ( { model | page = nextPage }, effect )

        ( GotDashboardMsg _, _ ) ->
            ( model, EffectNone )

        ( GotLoginMsg loginMsg, Login page ) ->
            let
                ( nextPage, appAction, effect ) =
                    Login.update model.shared loginMsg page
                        |> Tuple3.mapFirst Login
                        |> Tuple3.mapThird EffectLogin
            in
            withAppAction appAction ( { model | page = nextPage }, effect )
                |> (\( nextModel, nextEffect ) ->
                        case loginMsg of
                            Login.ReceivedLoginResponse (Ok res) ->
                                let
                                    user : User
                                    user =
                                        { userId = UserId res.userId }
                                in
                                ( { nextModel | user = HttpData.Success (Just user) }
                                , nextEffect
                                )

                            Login.ReceivedLoginResponse (Err err) ->
                                ( { nextModel | user = HttpData.Failure err }, nextEffect )

                            _ ->
                                ( nextModel, nextEffect )
                   )

        ( GotLoginMsg _, _ ) ->
            ( model, EffectNone )

        ( CheckedUserAuthorization redirectUrl (Ok (Just ( userId, token ))), _ ) ->
            ( { model | user = HttpData.Success (Just { userId = userId }) }
            , EffectBatch
                [ EffectReplaceUrl redirectUrl
                , EffectStartRealm token
                ]
            )

        ( CheckedUserAuthorization _ (Ok Nothing), _ ) ->
            ( { model | user = HttpData.Success Nothing }
            , EffectReplaceUrl "/app/login"
            )

        ( CheckedUserAuthorization _ (Err _), _ ) ->
            ( model, EffectNone )
                |> withAppAction
                    (Just <|
                        AppAction.ShowNotification
                            AppAction.NotificationError
                            (Translations.checkAuthorizationError model.shared.language)
                    )

        ( RouteChanged route, Dashboard _ page ) ->
            withAppAction (Dashboard.unload page) ( model, EffectNone )
                |> withRoute route

        ( RouteChanged route, Login page ) ->
            withAppAction (Login.unload page) ( model, EffectNone )
                |> withRoute route

        ( RouteChanged route, NotFound ) ->
            ( model, EffectNone )
                |> withRoute route

        ( RouteChanged route, CheckAuth ) ->
            ( model, EffectNone )
                |> withRoute route


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.linkClicked (Routes.parseUrl >> RouteChanged)
        , case model.page of
            Login _ ->
                Sub.none

            Dashboard _ page ->
                Dashboard.subscriptions page
                    |> Sub.map GotDashboardMsg

            CheckAuth ->
                Sub.none

            NotFound ->
                Sub.none
        ]


view : Model -> Html Msg
view model =
    Layout.view GotLayoutMsg model.notification model.shared model.layout <|
        case model.page of
            Login page ->
                Login.view model.shared page
                    |> H.map GotLoginMsg

            Dashboard _ page ->
                Dashboard.view model.shared page
                    |> H.map GotDashboardMsg

            CheckAuth ->
                H.text "Authorizing User"

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
