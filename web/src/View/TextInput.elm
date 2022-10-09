module View.TextInput exposing (Config, InitialConfig, config, view, withErrorMessage, withOnInput, withValue)

import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Maybe.Extra as Maybe
import Theme exposing (Theme)


type Config msg
    = Config (Config_ msg)


type alias Config_ msg =
    { label : String
    , id : String
    , value : String
    , onInput : Maybe (String -> msg)
    , errorMessage : Maybe String
    , theme : Theme
    }


withValue : String -> Config msg -> Config msg
withValue value (Config cfg) =
    Config { cfg | value = value }


withOnInput : Maybe (String -> msg) -> Config msg -> Config msg
withOnInput onInput (Config cfg) =
    Config { cfg | onInput = onInput }


withErrorMessage : Maybe String -> Config msg -> Config msg
withErrorMessage errorMessage (Config cfg) =
    Config { cfg | errorMessage = errorMessage }


type alias InitialConfig =
    { label : String
    , id : String
    , theme : Theme
    }


config : InitialConfig -> Config msg
config initialConfig =
    Config
        { label = initialConfig.label
        , id = initialConfig.id
        , theme = initialConfig.theme
        , value = ""
        , onInput = Nothing
        , errorMessage = Nothing
        }


view : Config msg -> Html msg
view (Config cfg) =
    let
        disabled : Bool
        disabled =
            Maybe.isNothing cfg.onInput
    in
    H.div
        [ A.css
            [ Css.display Css.inlineFlex
            , Css.flexDirection Css.column
            ]
        ]
        [ H.label
            [ A.for cfg.id
            , A.css
                [ Css.marginBottom (Css.rem 0.5)
                , Css.color cfg.theme.widgetFocusOutline
                ]
            ]
            [ H.text cfg.label
            ]
        , H.input
            ([ A.disabled disabled
             , A.value cfg.value
             , A.id cfg.id
             , A.css
                [ Css.padding2 (Css.rem 0.5) (Css.rem 0.5)
                , Css.fontSize (Css.rem 1)
                , Css.focus
                    [ Css.outlineOffset (Css.rem 0.125)
                    , Css.outline3 (Css.rem 0.125) Css.solid cfg.theme.widgetFocusOutline
                    ]
                ]
             ]
                ++ (case cfg.onInput of
                        Just onInput ->
                            [ E.onInput onInput ]

                        Nothing ->
                            []
                   )
            )
            []
        ]
