module AuthTest exposing (suite)

import Main
import ProgramTest
import Test exposing (Test, describe, test)
import TestApp exposing (testProgram)
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
                    |> ProgramTest.expectPageChange
                        (Url.Builder.crossOrigin TestApp.baseUrl [ "app", "login" ] [])
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
                    |> ProgramTest.expectPageChange
                        (Url.Builder.crossOrigin TestApp.baseUrl [ "app", "login" ] [])
        ]
