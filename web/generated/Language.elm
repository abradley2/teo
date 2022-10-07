module Language exposing (..)

{-| 
-}


import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( ""
          , I18Next.object
                [ ( "check authorization error"
                  , I18Next.string
                        "There was an error when checking your user authorization."
                  )
                , ( "dashboard"
                  , I18Next.object
                        [ ( "greeting", I18Next.string "Hello, {{name}}" ) ]
                  )
                , ( "login"
                  , I18Next.object
                        [ ( "button prompt"
                          , I18Next.string "Click here to login"
                          )
                        ]
                  )
                ]
          )
        ]


