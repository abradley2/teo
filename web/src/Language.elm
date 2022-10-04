module Language exposing (..)

{-| 
-}


import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( ""
          , I18Next.object
                [ ( "hello", I18Next.string "world" )
                , ( "login"
                  , I18Next.object
                        [ ( "message", I18Next.string "Hello {{name}}" ) ]
                  )
                , ( "test Oneeee"
                  , I18Next.string "Just one {{replacement}} here"
                  )
                , ( "test Three"
                  , I18Next.string
                        "The {{adjective}} {{subject}} {{verb}} quickly"
                  )
                , ( "test Twos"
                  , I18Next.string
                        "I am a {{subject}} who is {{adjective}} test"
                  )
                ]
          )
        ]


