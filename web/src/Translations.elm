module Translations exposing (..)

{-| 
-}


import I18Next


hello : List I18Next.Translations -> String
hello translations =
    I18Next.tf translations "hello"


testOneeee : List I18Next.Translations -> { replacement : String } -> String
testOneeee translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "test Oneeee"
        [ ( "replacement", replacements.replacement ) ]


testThree :
    List I18Next.Translations
    -> { adjective : String, subject : String, verb : String }
    -> String
testThree translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "test Three"
        [ ( "adjective", replacements.adjective )
        , ( "subject", replacements.subject )
        , ( "verb", replacements.verb )
        ]


testTwos :
    List I18Next.Translations
    -> { subject : String, adjective : String }
    -> String
testTwos translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "test Twos"
        [ ( "subject", replacements.subject )
        , ( "adjective", replacements.adjective )
        ]


