module AuthTest exposing (suite)

import Expect
import Language exposing (defaultLanguage)
import List.Extra as List
import Main
import Maybe.Extra as Maybe
import Page.Login as Login
import ProgramTest
import Shared
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Http as SimulatedHttp
import Test exposing (Test, describe, test)
import Test.Http
import TestApp exposing (testProgram)
import Translations.Login
import Url.Builder


suite : Test
suite =
    describe "Auth"
        [ test "Unauthenticated users are redirected to the login page" <|
            \_ ->
                testProgram
                    |> TestApp.withBaseUrl
                    |> TestApp.withSimulatedEffects (always Nothing)
                    |> TestApp.startWithFlags identity
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        Main.checkAuthUrl
                        """{"authorized": false}"""
                    |> ProgramTest.expectBrowserUrl
                        (Expect.equal <| Url.Builder.crossOrigin TestApp.baseUrl [ "app", "login" ] [])
        , test "Users can log in by clicking the login button" <|
            \_ ->
                testProgram
                    |> TestApp.withBaseUrl
                    |> TestApp.withSimulatedEffects (always Nothing)
                    |> TestApp.startWithFlags identity
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        Main.checkAuthUrl
                        """{"authorized": false}"""
                    |> ProgramTest.clickButton (Translations.Login.buttonPrompt [ defaultLanguage ])
                    |> ProgramTest.simulateHttpOk
                        "POST"
                        Login.loginUrl
                        """{"authorized": true}"""
                    |> ProgramTest.expectBrowserUrl
                        (Expect.equal <| Url.Builder.crossOrigin TestApp.baseUrl [] [])
        , test "An error is displayed if the initial check auth request fails" <|
            \_ ->
                testProgram
                    |> TestApp.withBaseUrl
                    |> TestApp.withSimulatedEffects (always Nothing)
                    |> TestApp.startWithFlags identity
                    |> ProgramTest.simulateHttpResponse
                        "GET"
                        Main.checkAuthUrl
                        Test.Http.networkError
                    |> always Expect.pass
        ]
