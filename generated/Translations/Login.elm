module Translations.Login exposing (..)

{-| 
-}


import I18Next


buttonPrompt : List I18Next.Translations -> String
buttonPrompt translations =
    I18Next.tf translations "login.buttonPrompt"


