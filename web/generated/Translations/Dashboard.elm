module Translations.Dashboard exposing (..)

{-| 
-}


import I18Next


greeting : List I18Next.Translations -> { name : String } -> String
greeting translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "dashboard.greeting"
        [ ( "name", replacements.name ) ]


