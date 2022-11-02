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


userIdEmptyError : List I18Next.Translations -> String
userIdEmptyError translations =
    I18Next.tf translations "login.user id empty error"


userIdInputGenericError : List I18Next.Translations -> String
userIdInputGenericError translations =
    I18Next.tf translations "login.user id input generic error"


userIdInputLabel : List I18Next.Translations -> String
userIdInputLabel translations =
    I18Next.tf translations "login.user id input label"


userIdTooLongError : List I18Next.Translations -> String
userIdTooLongError translations =
    I18Next.tf translations "login.user id too long error"


userIdTooShortError : List I18Next.Translations -> String
userIdTooShortError translations =
    I18Next.tf translations "login.user id too short error"


