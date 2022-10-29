module SimulatedEffect.Page.Dashboard exposing (..)

import Page.Dashboard as Dashboard
import ProgramTest exposing (SimulatedEffect)
import SimulatedEffect.Cmd as SimulatedCmd


perform : Dashboard.Effect -> SimulatedEffect Dashboard.Msg
perform effect =
    case effect of
        _ ->
            SimulatedCmd.none
