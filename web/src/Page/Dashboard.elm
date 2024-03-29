module Page.Dashboard exposing (..)

import AppAction exposing (AppAction)
import Html.Styled as H exposing (Html)
import Http
import HttpData exposing (HttpData(..), RequestTag(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports
import Shared exposing (Shared)
import Translations.Dashboard
import User exposing (User, UserId(..))
import View.TextInput as TextInput


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
    | ReceivedParticipatingEventsResponse (Result Http.Error (List Event))
    | EventCodeEntryChanged String
    | ReceivedData Value


type Effect
    = EffectNone
    | EffectRequestEvents RequestTag UserId
    | EffectRequestParticipatingEvents RequestTag UserId
    | EffectBatch (List Effect)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectRequestEvents (RequestTag tag) (UserId userId) ->
            Ports.requestEvents
                (Encode.object
                    [ ( "userId", Encode.string userId )
                    , ( "tag", Encode.string tag )
                    ]
                )

        EffectRequestParticipatingEvents (RequestTag tag) (UserId userId) ->
            Ports.requestParticipatingEvents
                (Encode.object
                    [ ( "userId", Encode.string userId )
                    , ( "tag", Encode.string tag )
                    ]
                )

        EffectBatch effects ->
            List.foldr
                (\effect_ cmd -> Cmd.batch [ perform effect_, cmd ])
                Cmd.none
                effects

        EffectNone ->
            Cmd.none


type alias Model =
    { events : HttpData (List Event)
    , participatingEvents : HttpData (List Event)
    , eventCodeEntry : String
    }


eventCodeEntryId : String
eventCodeEntryId =
    "event-code-entry-input"


dataKey : String
dataKey =
    "dashboard-data"


init : User -> Shared -> ( Model, Maybe AppAction, Effect )
init user shared =
    ( { events = Loading Nothing
      , participatingEvents = Loading Nothing
      , eventCodeEntry = ""
      }
    , Just (AppAction.RequestData dataKey)
    , EffectBatch
        [ EffectRequestEvents requestEventsTag user.userId
        , EffectRequestParticipatingEvents requestParticipatingEventsTag user.userId
        ]
    )


requestEventsTag : RequestTag
requestEventsTag =
    RequestTag "Page.Dashboard.requestEvents"


requestParticipatingEventsTag : RequestTag
requestParticipatingEventsTag =
    RequestTag "Page.Dashboard.requestParticipatingEvents"


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
        , HttpData.httpResponseSub
            { tag = requestEventsTag
            , ignoreMsg = NoOp
            , toMsg = ReceivedEventsResponse
            }
            (Decode.list eventDecoder)
            |> Ports.requestEventsResponse
        , HttpData.httpResponseSub
            { tag = requestParticipatingEventsTag
            , ignoreMsg = NoOp
            , toMsg = ReceivedParticipatingEventsResponse
            }
            (Decode.list eventDecoder)
            |> Ports.requestParticipatingEventsResponse
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
    case msg of
        NoOp ->
            ( model, Nothing, EffectNone )

        ReceivedEventsResponse (Err err) ->
            ( { model | events = Failure err }
            , Just
                (AppAction.ShowNotification
                    (AppAction.NotificationError << Just <| HttpData.httpErrorToString err)
                    (Translations.Dashboard.eventFetchFailure shared.language)
                )
            , EffectNone
            )

        ReceivedEventsResponse (Ok res) ->
            ( { model
                | events = Success res
              }
            , Nothing
            , EffectNone
            )

        ReceivedParticipatingEventsResponse (Ok res) ->
            ( { model
                | participatingEvents = Success res
              }
            , Nothing
            , EffectNone
            )

        ReceivedParticipatingEventsResponse (Err err) ->
            ( { model
                | participatingEvents = Failure err
              }
            , Just <|
                AppAction.ShowNotification
                    (AppAction.NotificationError (Just <| HttpData.httpErrorToString err))
                    (Translations.Dashboard.eventFetchFailure shared.language)
            , EffectNone
            )

        ReceivedData _ ->
            ( model, Nothing, EffectNone )

        EventCodeEntryChanged eventCodeEntry ->
            ( { model
                | eventCodeEntry = eventCodeEntry
              }
            , Nothing
            , EffectNone
            )


view : Shared -> Model -> Html Msg
view shared model =
    H.div
        []
        [ H.text <|
            Translations.Dashboard.greeting shared.language { name = "Tony" }
        , H.div
            []
            [ let
                config : TextInput.InitialConfig
                config =
                    { theme = shared.theme
                    , label = "Event Code"
                    , id = eventCodeEntryId
                    }
              in
              TextInput.config config
                |> TextInput.withValue model.eventCodeEntry
                |> TextInput.withOnInput (Just EventCodeEntryChanged)
                |> TextInput.view
            ]
        ]
