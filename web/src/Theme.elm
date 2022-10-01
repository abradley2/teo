module Theme exposing (Theme, darkTheme)

import Css


type alias Theme =
    { bodyBackground : Css.Color
    , bodySecondaryBackground : Css.Color
    , bodyForeground : Css.Color
    , bodySecondaryForeground : Css.Color
    , bodyForegroundBorder : Css.Color
    , bodySecondaryForegroundBorder : Css.Color
    , bodyFont : Css.Color
    , bodyForegroundFont : Css.Color
    }


darkTheme : Theme
darkTheme =
    { bodyBackground = Css.hex "#252e37"
    , bodySecondaryBackground = Css.hex "#2e3a46"
    , bodyForeground = Css.hex "#3b4a5a"
    , bodySecondaryForeground = Css.hex "#4a5b6c"
    , bodyForegroundBorder = Css.hex "#5a6c7e"
    , bodySecondaryForegroundBorder = Css.hex "#6a7d90"
    , bodyFont = Css.hex "#7a8ea2"
    , bodyForegroundFont = Css.hex "#8a9fb4"
    }
