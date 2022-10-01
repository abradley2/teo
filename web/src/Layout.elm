module Layout exposing (..)

import Css
import Css.Transitions as Transitions
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Shared exposing (Shared)


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


view : (Msg -> msg) -> Shared -> Model -> Html msg -> Html msg
view toMsg shared model body =
    let
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
                [ Css.displayFlex
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.alignItems Css.stretch
                ]
            ]
            [ H.div
                [ let
                    cssAttrs =
                        if model.menuOpen then
                            [ Css.minWidth (Css.rem 16)
                            , Css.maxWidth (Css.rem 16)
                            ]

                        else
                            [ Css.minWidth (Css.rem 6)
                            , Css.maxWidth (Css.rem 6)
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
                    [ Css.minWidth <| Css.calc (Css.pct 100) Css.minus (Css.rem 8)
                    , Css.maxWidth <| Css.calc (Css.pct 100) Css.minus (Css.rem 8)
                    , Css.overflow Css.auto
                    ]
                ]
                [ body
                ]
            ]
        ]


sidebar : Shared -> Html Msg
sidebar shared =
    H.div
        [ E.onClick ToggleMenu
        , A.css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        ]
        []
