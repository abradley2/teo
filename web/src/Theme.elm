module Theme exposing (Theme, darkTheme)

import Css


type alias Theme =
    { -- body represents base page-level items
      bodyBackground : Css.Color
    , bodyFont : Css.Color
    , bodySecondaryBackground : Css.Color

    -- the body foreground represents things like opening up menus,
    -- cards, and sidebars
    -- that sit just above the main body content
    , bodyForeground : Css.Color
    , bodyForegroundFont : Css.Color
    , bodyForegroundBorder : Css.Color

    -- the body secondary foreground represents things that may sit
    -- in front of the application, like modals, toasts, and notifications
    , bodySecondaryForeground : Css.Color
    , bodySecondaryForegroundFont : Css.Color
    , bodySecondaryForegroundBorder : Css.Color

    -- primary action represents things like a "submit button"
    , primaryActionBackground : Css.Color
    , primaryActionFont : Css.Color

    -- widget focus outline should contrast well against all body backgrounds
    , widgetFocusOutline : Css.Color
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
    , bodySecondaryForegroundFont = Css.hex "#d0d9e1"
    , primaryActionBackground = Css.hex "#1e88e5"
    , primaryActionFont = Css.hex "#ffffff"
    , widgetFocusOutline = Css.hex "#f5f7fa"
    }
