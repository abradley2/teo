module Theme exposing (..)

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


lightTheme : Theme
lightTheme =
    { bodyBackground = Css.hex "#f5f5f5"
    , bodySecondaryBackground = Css.hex "#e5e5e5"
    , bodyForeground = Css.hex "#d5d5d5"
    , bodySecondaryForeground = Css.hex "#c5c5c5"
    , bodyForegroundBorder = Css.hex "#b5b5b5"
    , bodySecondaryForegroundBorder = Css.hex "#a5a5a5"
    , bodyFont = Css.hex "#959595"
    , bodyForegroundFont = Css.hex "#858585"
    }
