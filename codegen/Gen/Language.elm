module Gen.Language exposing (annotation_, call_, getWithFallback, moduleName_, values_)

{-| 
@docs values_, call_, annotation_, getWithFallback, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Language" ]


{-| getWithFallback: Dict String String -> Dict String String -> String -> String -}
getWithFallback : Elm.Expression -> Elm.Expression -> String -> Elm.Expression
getWithFallback getWithFallbackArg getWithFallbackArg0 getWithFallbackArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Language" ]
            , name = "getWithFallback"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Dict" [ Type.string, Type.string ]
                        , Type.namedWith [] "Dict" [ Type.string, Type.string ]
                        , Type.string
                        ]
                        Type.string
                    )
            }
        )
        [ getWithFallbackArg
        , getWithFallbackArg0
        , Elm.string getWithFallbackArg1
        ]


annotation_ : { language : Type.Annotation }
annotation_ =
    { language =
        Type.alias
            moduleName_
            "Language"
            []
            (Type.namedWith [] "Dict" [ Type.string, Type.string ])
    }


call_ :
    { getWithFallback :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { getWithFallback =
        \getWithFallbackArg getWithFallbackArg0 getWithFallbackArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Language" ]
                    , name = "getWithFallback"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    []
                                    "Dict"
                                    [ Type.string, Type.string ]
                                , Type.namedWith
                                    []
                                    "Dict"
                                    [ Type.string, Type.string ]
                                , Type.string
                                ]
                                Type.string
                            )
                    }
                )
                [ getWithFallbackArg, getWithFallbackArg0, getWithFallbackArg1 ]
    }


values_ : { getWithFallback : Elm.Expression }
values_ =
    { getWithFallback =
        Elm.value
            { importFrom = [ "Language" ]
            , name = "getWithFallback"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [] "Dict" [ Type.string, Type.string ]
                        , Type.namedWith [] "Dict" [ Type.string, Type.string ]
                        , Type.string
                        ]
                        Type.string
                    )
            }
    }


