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
                , ( "testOne", I18Next.string "Just one {{replacement}} here" )
                , ( "testThree"
                  , I18Next.string
                        "The {{adjective}} {{subject}} {{verb}} quickly"
                  )
                , ( "testTwo"
                  , I18Next.string
                        "I am a {{subject}} who is {{adjective}} test"
                  )
                ]
          )
        ]


