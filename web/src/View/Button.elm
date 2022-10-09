module View.Button exposing (Config, InitialConfig, config, view, withOnClick)

import Css
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Maybe.Extra as Maybe
import Theme exposing (Theme)


type Config msg
    = Config (Config_ msg)


type alias Config_ msg =
    { label : String
    , theme : Theme
    , onClick : Maybe msg
    }


withOnClick : msg -> Config msg -> Config msg
withOnClick msg (Config cfg) =
    Config { cfg | onClick = Just msg }


type alias InitialConfig =
    { label : String
    , theme : Theme
    }


config : InitialConfig -> Config msg
config cfg =
    Config
        { label = cfg.label
        , onClick = Nothing
        , theme = cfg.theme
        }


view : Config msg -> Html msg
view (Config cfg) =
    let
        disabled : Bool
        disabled =
            Maybe.isNothing cfg.onClick

        attrs : List (Attribute msg)
        attrs =
            case cfg.onClick of
                Just onClick ->
                    [ E.onClick onClick
                    ]

                Nothing ->
                    []
    in
    H.button
        ([ A.type_ "button"
         , A.disabled disabled
         , A.css
            [ Css.backgroundColor cfg.theme.primaryActionBackground
            , Css.color cfg.theme.primaryActionFont
            , Css.borderStyle Css.none
            , Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.padding2 (Css.rem 0) (Css.rem 1)
            , Css.fontSize (Css.rem 1)
            , Css.cursor Css.pointer
            , Css.height (Css.rem 2.5)
            , Css.focus
                [ Css.outlineOffset (Css.rem 0.125)
                , Css.outline3 (Css.rem 0.125) Css.solid cfg.theme.widgetFocusOutline
                ]
            ]
         ]
            ++ attrs
        )
        [ H.text cfg.label
        ]
