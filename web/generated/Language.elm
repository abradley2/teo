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
                        , ( "login failed message"
                          , I18Next.string "Login failed. Please try again."
                          )
                        , ( "user id input generic error"
                          , I18Next.string "Failed to login"
                          )
                        , ( "user id input label"
                          , I18Next.string "Enter a (completely fake) user ID"
                          )
                        ]
                  )
                , ( "page not found message"
                  , I18Next.string
                        "Sorry, the page you are looking for doesn't exist"
                  )
                ]
          )
        ]


