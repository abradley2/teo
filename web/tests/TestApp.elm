module TestApp exposing (..)

import Browser
import Html.Styled
import I18Next
import List.Nonempty exposing (Nonempty(..))
import Main
import ProgramTest exposing (ProgramDefinition, ProgramTest, SimulatedEffect)
import Routes
import Shared exposing (ClientId(..))
import SimulatedEffect.Main
import Url


type alias MainProgramDefinition =
    ProgramDefinition Main.Flags Main.Model Main.Msg Main.Effect


type alias MainProgramTest =
    ProgramTest Main.Model Main.Msg Main.Effect


baseUrl : String
baseUrl =
    "http://localhost:1234"


defaultFlags : Main.Flags
defaultFlags =
    { clientId = ClientId "test-client-id"
    , languages = Nonempty ( Shared.EN, I18Next.initialTranslations ) []
    , url = baseUrl
    }


startWithFlags : (Main.Flags -> Main.Flags) -> MainProgramDefinition -> MainProgramTest
startWithFlags flagsModifier =
    ProgramTest.start (flagsModifier defaultFlags)


withBaseUrl : MainProgramDefinition -> MainProgramDefinition
withBaseUrl =
    ProgramTest.withBaseUrl baseUrl


withSimulatedEffects : (Main.Effect -> Maybe (SimulatedEffect Main.Msg)) -> MainProgramDefinition -> MainProgramDefinition
withSimulatedEffects transform =
    ProgramTest.withSimulatedEffects (\effect -> transform effect |> Maybe.withDefault (SimulatedEffect.Main.perform <| Debug.log "PERFORM EFFECT" effect))


testProgram : MainProgramDefinition
testProgram =
    ProgramTest.createApplication
        { init = \flags _ _ -> Main.initWithFlags flags
        , onUrlRequest = always Main.NoOp
        , onUrlChange = Url.toString >> Routes.parseUrl >> Main.RouteChanged
        , update = Main.update
        , view = Main.view >> Html.Styled.toUnstyled >> List.singleton >> Browser.Document "test-app"
        }
