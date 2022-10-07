module SimulatedEffect.Main exposing (..)

import Json.Encode as Encode
import Main
import ProgramTest exposing (SimulatedEffect)
import Shared exposing (ClientId(..))
import SimulatedEffect.Cmd as SimulatedCmd
import SimulatedEffect.Http as SimulatedHttp
import SimulatedEffect.Navigation as SimulatedNavigation
import SimulatedEffect.Page.Dashboard
import SimulatedEffect.Page.Login
import SimulatedEffect.Ports as SimulatedPorts


perform : Main.Effect -> SimulatedEffect Main.Msg
perform effect =
    case effect of
        Main.EffectNone ->
            SimulatedCmd.none

        Main.EffectDashboard subEffect ->
            SimulatedEffect.Page.Dashboard.perform subEffect
                |> SimulatedCmd.map Main.GotDashboardMsg

        Main.EffectLogin subEffect ->
            SimulatedEffect.Page.Login.perform subEffect
                |> SimulatedCmd.map Main.GotLoginMsg

        Main.EffectCheckAuth (ClientId clientId) redirectUrl ->
            SimulatedHttp.request
                { method = "GET"
                , url = Main.checkAuthUrl
                , body = SimulatedHttp.emptyBody
                , tracker = Nothing
                , expect =
                    SimulatedHttp.expectJson
                        (Main.CheckedUserAuthorization redirectUrl)
                        Main.checkAuthResponseDecoder
                , headers =
                    [ SimulatedHttp.header "X-Client-Id" clientId
                    ]
                , timeout = Just 5000
                }

        Main.EffectCancelRequest tracker ->
            SimulatedCmd.none

        Main.EffectReplaceUrl url ->
            SimulatedNavigation.replaceUrl url

        Main.EffectPushUrl url ->
            SimulatedNavigation.pushUrl url

        Main.EffectStoreData key value ->
            SimulatedPorts.send "storeData"
                (Encode.list
                    identity
                    [ Encode.string key, value ]
                )

        Main.EffectRequestData key ->
            SimulatedPorts.send "requestData" (Encode.string key)

        Main.EffectBatch effects ->
            List.foldr
                (\eff batch -> SimulatedCmd.batch [ perform eff, batch ])
                SimulatedCmd.none
                effects
