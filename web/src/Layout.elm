module Layout exposing (Model, Msg(..), init, update, view)

import AppAction exposing (Notification)
import Css exposing (Style)
import Css.Transitions as Transitions
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Shared exposing (Shared)
import Theme exposing (Theme)


type Msg
    = ToggleMenu


type alias Model =
    { menuOpen : Bool
    }


init : Model
init =
    { menuOpen = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleMenu ->
            { model | menuOpen = not model.menuOpen }


view : (Msg -> msg) -> Maybe ( Notification, String ) -> Shared -> Model -> Html msg -> Html msg
view toMsg notification shared model body =
    let
        theme : Theme
        theme =
            shared.theme
    in
    H.div
        [ A.css
            [ Css.position Css.absolute
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.bottom (Css.px 0)
            , Css.right (Css.px 0)
            , Css.overflow Css.hidden
            , Css.backgroundColor theme.bodyBackground
            , Css.color theme.bodyFont
            , Css.fontFamilies [ "system-ui", "Avenir", "sans-serif" ]
            ]
        ]
        [ H.div
            [ A.css
                [ Css.position Css.fixed
                , Css.top (Css.rem 1)
                , Css.right (Css.rem 1)
                , Css.backgroundColor theme.bodySecondaryForeground
                , Css.color theme.bodySecondaryForegroundFont
                , Css.border3 (Css.px 1) Css.solid theme.bodySecondaryForegroundBorder
                ]
            ]
            [ case notification of
                Just ( _, notificationMsg ) ->
                    H.div
                        [ A.css
                            [ Css.position Css.relative
                            , Css.padding2 (Css.rem 2) (Css.rem 1)
                            ]
                        ]
                        [ H.text notificationMsg
                        , H.button
                            [ A.css
                                [ Css.top (Css.rem 0.25)
                                , Css.right (Css.rem 0.25)
                                , Css.position Css.absolute
                                ]
                            ]
                            [ H.text "X" ]
                        ]

                Nothing ->
                    H.text ""
            ]
        , H.div
            [ A.css
                [ Css.displayFlex
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.alignItems Css.stretch
                ]
            ]
            [ H.div
                [ let
                    cssAttrs : List Style
                    cssAttrs =
                        if model.menuOpen then
                            [ Css.minWidth (Css.rem 16)
                            , Css.maxWidth (Css.rem 16)
                            ]

                        else
                            [ Css.minWidth (Css.rem 5)
                            , Css.maxWidth (Css.rem 5)
                            ]
                  in
                  A.css <|
                    [ Css.backgroundColor theme.bodyForeground
                    , Css.color theme.bodyForegroundFont
                    , Transitions.transition
                        [ Transitions.maxWidth 200
                        , Transitions.minWidth 200
                        ]
                    ]
                        ++ cssAttrs
                ]
                [ H.map toMsg (sidebar shared)
                ]
            , H.div
                [ A.css
                    [ Css.minWidth <| Css.calc (Css.pct 100) Css.minus (Css.rem 5)
                    , Css.maxWidth <| Css.calc (Css.pct 100) Css.minus (Css.rem 5)
                    , Css.overflow Css.auto
                    , Css.boxSizing Css.borderBox
                    , Css.padding (Css.rem 1)
                    ]
                ]
                [ body
                ]
            ]
        ]


sidebar : Shared -> Html Msg
sidebar _ =
    H.div
        [ E.onClick ToggleMenu
        , A.css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        ]
        []
