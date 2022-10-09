module Translations.Login exposing (..)

{-| 
-}


import I18Next


buttonPrompt : List I18Next.Translations -> String
buttonPrompt translations =
    I18Next.tf translations "login.button prompt"


loginFailedMessage : List I18Next.Translations -> String
loginFailedMessage translations =
    I18Next.tf translations "login.login failed message"


userIdInputGenericError : List I18Next.Translations -> String
userIdInputGenericError translations =
    I18Next.tf translations "login.user id input generic error"


userIdInputLabel : List I18Next.Translations -> String
userIdInputLabel translations =
    I18Next.tf translations "login.user id input label"


