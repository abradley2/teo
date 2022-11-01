module AuthTest exposing (suite)

import Expect
import Language exposing (defaultLanguage)
import Main
import Page.Login as Login
import ProgramTest
import Test exposing (Test, describe, test)
import Test.Html.Selector as Selector
import Test.Http
import TestApp exposing (testProgram)
import Translations
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
                        """{"token": null, "userId": null}"""
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
                        """{"token": null, "userId": null}"""
                    |> ProgramTest.clickButton (Translations.Login.buttonPrompt [ defaultLanguage ])
                    |> ProgramTest.simulateHttpOk
                        "POST"
                        Login.loginUrl
                        """{"token": "fake-token", "userId": "Test user"}"""
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
                    |> ProgramTest.expectViewHas
                        [ Selector.text (Translations.checkAuthorizationError [ defaultLanguage ])
                        ]
        , test "An error is displayed if the request after a user tries to login fails" <|
            \_ ->
                testProgram
                    |> TestApp.withBaseUrl
                    |> TestApp.withSimulatedEffects (always Nothing)
                    |> TestApp.startWithFlags identity
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        Main.checkAuthUrl
                        """{"token": null, "userId": null}"""
                    |> ProgramTest.clickButton (Translations.Login.buttonPrompt [ defaultLanguage ])
                    |> ProgramTest.simulateHttpResponse
                        "POST"
                        Login.loginUrl
                        Test.Http.networkError
                    |> ProgramTest.expectViewHas
                        [ Selector.text (Translations.Login.loginFailedMessage [ defaultLanguage ])
                        ]
        ]
