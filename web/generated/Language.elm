module Language exposing (..)

{-| 
-}


import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( ""
          , I18Next.object
                [ ( "dashboard"
                  , I18Next.object
                        [ ( "greeting", I18Next.string "Hello, {{name}}" ) ]
                  )
                , ( "login"
                  , I18Next.object
                        [ ( "buttonPrompt"
                          , I18Next.string "Click here to login"
                          )
                        ]
                  )
                ]
          )
        ]


