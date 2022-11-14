module DashboardTest exposing (..)

import Expect
import HttpData exposing (RequestTag(..))
import Main
import Page.Dashboard as Dashboard
import ProgramTest
import Routes
import Test exposing (Test, describe, test)
import TestApp exposing (testProgram)
import Url.Builder


suite : Test
suite =
    describe "Dashboard"
        [ test "We fetch both events and participating events on initialization" <|
            \_ ->
                testProgram
                    |> TestApp.withBaseUrl
                    |> TestApp.withSimulatedEffects (always Nothing)
                    |> TestApp.startWithFlags identity
                    |> ProgramTest.update (Main.RouteChanged Routes.Dashboard)
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        Main.checkAuthUrl
                        """{"token": "fake-token", "userId": "test-user"}"""
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        (Url.Builder.absolute [ "/api" ]
                            [ Url.Builder.string "tag"
                                (case Dashboard.requestEventsTag of
                                    RequestTag tag ->
                                        tag
                                )
                            ]
                        )
                        """[]"""
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        (Url.Builder.absolute
                            [ "/apis" ]
                            [ Url.Builder.string "tag"
                                (case Dashboard.requestEventsTag of
                                    RequestTag tag ->
                                        tag
                                )
                            ]
                        )
                        """[]"""
                    |> always Expect.pass
        ]
