module Translations.Login exposing (..)

{-| 
-}


import I18Next


message : List I18Next.Translations -> { name : String } -> String
message translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "login.message"
        [ ( "name", replacements.name ) ]


