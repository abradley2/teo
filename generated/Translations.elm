module Translations exposing (..)

{-| 
-}


import I18Next


hello : List I18Next.Translations -> String
hello translations =
    I18Next.tf translations "hello"


testOne : List I18Next.Translations -> { replacement : String } -> String
testOne translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "testOne"
        [ ( "replacement", replacements.replacement ) ]


testThree :
    List I18Next.Translations
    -> { adjective : String, subject : String, verb : String }
    -> String
testThree translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "testThree"
        [ ( "adjective", replacements.adjective )
        , ( "subject", replacements.subject )
        , ( "verb", replacements.verb )
        ]


testTwo :
    List I18Next.Translations
    -> { subject : String, adjective : String }
    -> String
testTwo translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "testTwo"
        [ ( "subject", replacements.subject )
        , ( "adjective", replacements.adjective )
        ]


