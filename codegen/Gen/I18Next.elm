module Gen.I18Next exposing (values_, call_, caseOf_, make_, annotation_, object, string, fromTree, hasKey, keys, customTrf, customTr, trf, tf, tr, t, translationsDecoder, initialTranslations, moduleName_)

{-|

@docs values_, call_, caseOf_, make_, annotation_, object, string, fromTree, hasKey, keys, customTrf, customTr, trf, tf, tr, t, translationsDecoder, initialTranslations, moduleName_

-}

import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module.
-}
moduleName_ : List String
moduleName_ =
    [ "I18Next" ]


{-| Use this to initialize Translations in your model. This may be needed
when loading translations but you need to initialize your model before
your translations are fetched.

initialTranslations: I18Next.Translations

-}
initialTranslations : Elm.Expression
initialTranslations =
    Elm.value
        { importFrom = [ "I18Next" ]
        , name = "initialTranslations"
        , annotation = Just (Type.namedWith [ "I18Next" ] "Translations" [])
        }


{-| Decode a JSON translations file. The JSON can be arbitrarly nested, but the
leaf values can only be strings. Use this decoder directly, if you are passing
the translations JSON into your elm app via flags or ports.
After decoding nested values will be available with any of the translate
functions separated with dots.

    {- The JSON could look like this:
    {
      "buttons": {
        "save": "Save",
        "cancel": "Cancel"
      },
      "greetings": {
        "hello": "Hello",
        "goodDay": "Good Day {{firstName}} {{lastName}}"
      }
    }
    -}

    --Use the decoder like this on a string
    import I18Next exposing (translationsDecoder)
    Json.Decode.decodeString translationsDecoder "{ \"greet\": \"Hello\" }"

    -- or on a Json.Encode.Value
    Json.Decode.decodeValue translationsDecoder encodedJson

translationsDecoder: Json.Decode.Decoder I18Next.Translations

-}
translationsDecoder : Elm.Expression
translationsDecoder =
    Elm.value
        { importFrom = [ "I18Next" ]
        , name = "translationsDecoder"
        , annotation =
            Just
                (Type.namedWith
                    [ "Json", "Decode" ]
                    "Decoder"
                    [ Type.namedWith [ "I18Next" ] "Translations" [] ]
                )
        }


{-| Translate a value at a given string.

    {- If your translations are { "greet": { "hello": "Hello" } }
    use dots to access nested keys.
    -}
    import I18Next exposing (t)
    t translations "greet.hello" -- "Hello"

t: I18Next.Translations -> String -> String

-}
t : Elm.Expression -> String -> Elm.Expression
t tArg tArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "t"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.string
                        ]
                        Type.string
                    )
            }
        )
        [ tArg, Elm.string tArg0 ]


{-| Translate a value at a key, while replacing placeholders.
Check the [`Delims`](I18Next#Delims) type for
reference how to specify placeholder delimiters.
Use this when you need to replace placeholders.

    -- If your translations are { "greet": "Hello {{name}}" }
    import I18Next exposing (tr, Delims(..))
    tr translations Curly "greet" [("name", "Peter")]

tr:
I18Next.Translations
-> I18Next.Delims
-> String
-> I18Next.Replacements
-> String

-}
tr :
    Elm.Expression
    -> Elm.Expression
    -> String
    -> Elm.Expression
    -> Elm.Expression
tr trArg trArg0 trArg1 trArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "tr"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.string
                        , Type.namedWith [ "I18Next" ] "Replacements" []
                        ]
                        Type.string
                    )
            }
        )
        [ trArg, trArg0, Elm.string trArg1, trArg2 ]


{-| Translate a value and try different fallback languages by providing a list
of Translations. If the key you provide does not exist in the first of the list
of languages, the function will try each language in the list.

    {- Will use german if the key exists there, or fall back to english
    if not. If the key is not in any of the provided languages the function
    will return the key. -}
    import I18Next exposing (tf)
    tf [germanTranslations, englishTranslations] "labels.greetings.hello"

tf: List I18Next.Translations -> String -> String

-}
tf : List Elm.Expression -> String -> Elm.Expression
tf tfArg tfArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "tf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "I18Next" ] "Translations" [])
                        , Type.string
                        ]
                        Type.string
                    )
            }
        )
        [ Elm.list tfArg, Elm.string tfArg0 ]


{-| Combines the [`tr`](I18Next#tr) and the [`tf`](I18Next#tf) function.
Only use this if you want to replace placeholders and apply fallback languages
at the same time.

    -- If your translations are { "greet": "Hello {{name}}" }
    import I18Next exposing (trf, Delims(..))
    let
      langList = [germanTranslations, englishTranslations]
    in
      trf langList Curly "greet" [("name", "Peter")] -- "Hello Peter"

trf:
List I18Next.Translations
-> I18Next.Delims
-> String
-> I18Next.Replacements
-> String

-}
trf :
    List Elm.Expression
    -> Elm.Expression
    -> String
    -> Elm.Expression
    -> Elm.Expression
trf trfArg trfArg0 trfArg1 trfArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "trf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "I18Next" ] "Translations" [])
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.string
                        , Type.namedWith [ "I18Next" ] "Replacements" []
                        ]
                        Type.string
                    )
            }
        )
        [ Elm.list trfArg, trfArg0, Elm.string trfArg1, trfArg2 ]


{-| Sometimes it can be useful to replace placeholders with other things than just `String`s.
Imagine you have translations containing a sentence with a link and you want to
provide the proper markup.
_Hint:_ The third argument is a function which will be called for any string pieces that
AREN'T placeholders, so that the types of replacements and the other other string parts match.
In most cases you'll just pass `Html.text` here.

    {- If your translations are { "call-to-action": "Go to {{elm-website}} for more information." }
    ...
    -}
    import Html exposing (text, a)

    customTr translationsEn Curly text "call-to-action" [ ( "elm-website", a [href "https://elm-lang.org"] [text "https://elm-lang.org"] ) ]
    -- Go to <a href="https://elm-lang.org">https://elm-lang.org</a> for more information.

If you only want `String`s though, use [`tr`](I18Next#tr) instead.

customTr:
I18Next.Translations
-> I18Next.Delims
-> (String -> a)
-> String
-> I18Next.CustomReplacements a
-> List a

-}
customTr :
    Elm.Expression
    -> Elm.Expression
    -> (Elm.Expression -> Elm.Expression)
    -> String
    -> Elm.Expression
    -> Elm.Expression
customTr customTrArg customTrArg0 customTrArg1 customTrArg2 customTrArg3 =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "customTr"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.function [ Type.string ] (Type.var "a")
                        , Type.string
                        , Type.namedWith
                            [ "I18Next" ]
                            "CustomReplacements"
                            [ Type.var "a" ]
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
        )
        [ customTrArg
        , customTrArg0
        , Elm.functionReduced "customTrUnpack" customTrArg1
        , Elm.string customTrArg2
        , customTrArg3
        ]


{-| Like [`customTr`](I18Next#customTr) but with support for fallback languages.

customTrf:
List I18Next.Translations
-> I18Next.Delims
-> (String -> a)
-> String
-> I18Next.CustomReplacements a
-> List a

-}
customTrf :
    List Elm.Expression
    -> Elm.Expression
    -> (Elm.Expression -> Elm.Expression)
    -> String
    -> Elm.Expression
    -> Elm.Expression
customTrf customTrfArg customTrfArg0 customTrfArg1 customTrfArg2 customTrfArg3 =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "customTrf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "I18Next" ] "Translations" [])
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.function [ Type.string ] (Type.var "a")
                        , Type.string
                        , Type.namedWith
                            [ "I18Next" ]
                            "CustomReplacements"
                            [ Type.var "a" ]
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
        )
        [ Elm.list customTrfArg
        , customTrfArg0
        , Elm.functionReduced "customTrfUnpack" customTrfArg1
        , Elm.string customTrfArg2
        , customTrfArg3
        ]


{-| Use this to obtain a list of keys that are contained in the translations.
From this it is simple to, for example, compare two translations for keys defined in one
but not the other. The order of the keys is arbitrary and should not be relied
on.

keys: I18Next.Translations -> List String

-}
keys : Elm.Expression -> Elm.Expression
keys keysArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "keys"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" [] ]
                        (Type.list Type.string)
                    )
            }
        )
        [ keysArg ]


{-| This function lets you check whether a certain key is exists in your
translations.

hasKey: I18Next.Translations -> String -> Bool

-}
hasKey : Elm.Expression -> String -> Elm.Expression
hasKey hasKeyArg hasKeyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "hasKey"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.string
                        ]
                        Type.bool
                    )
            }
        )
        [ hasKeyArg, Elm.string hasKeyArg0 ]


{-| Create a [`Translations`](I18Next#Translations) value from a list of pairs.

    import I18Next exposing (string, object, fromTree, t)

    translations =
        fromTree
          [ ("custom"
            , object
                [ ( "morning", string "Morning" )
                , ( "evening", string "Evening" )
                , ( "afternoon", string "Afternoon" )
                ]
            )
          , ("hello", string "hello")
          ]

    -- use it like this
    t translations "custom.morning" -- "Morning"

fromTree: List ( String, I18Next.Tree ) -> I18Next.Translations

-}
fromTree : List Elm.Expression -> Elm.Expression
fromTree fromTreeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "fromTree"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith [ "I18Next" ] "Tree" [])
                            )
                        ]
                        (Type.namedWith [ "I18Next" ] "Translations" [])
                    )
            }
        )
        [ Elm.list fromTreeArg ]


{-| Represents the leaf of a translations tree. It holds the actual translation
string.

string: String -> I18Next.Tree

-}
string : String -> Elm.Expression
string stringArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "I18Next" ] "Tree" [])
                    )
            }
        )
        [ Elm.string stringArg ]


{-| Let's you arange your translations in a hierarchy of objects.

object: List ( String, I18Next.Tree ) -> I18Next.Tree

-}
object : List Elm.Expression -> Elm.Expression
object objectArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "I18Next" ]
            , name = "object"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith [ "I18Next" ] "Tree" [])
                            )
                        ]
                        (Type.namedWith [ "I18Next" ] "Tree" [])
                    )
            }
        )
        [ Elm.list objectArg ]


annotation_ :
    { translations : Type.Annotation
    , delims : Type.Annotation
    , replacements : Type.Annotation
    , customReplacements : Type.Annotation -> Type.Annotation
    , tree : Type.Annotation
    }
annotation_ =
    { translations = Type.namedWith [ "I18Next" ] "Translations" []
    , delims = Type.namedWith [ "I18Next" ] "Delims" []
    , replacements =
        Type.alias
            moduleName_
            "Replacements"
            []
            (Type.list (Type.tuple Type.string Type.string))
    , customReplacements =
        \customReplacementsArg0 ->
            Type.alias
                moduleName_
                "CustomReplacements"
                [ customReplacementsArg0 ]
                (Type.list (Type.tuple Type.string (Type.var "a")))
    , tree = Type.namedWith [ "I18Next" ] "Tree" []
    }


make_ :
    { curly : Elm.Expression
    , underscore : Elm.Expression
    , custom : Elm.Expression -> Elm.Expression
    }
make_ =
    { curly =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "Curly"
            , annotation = Just (Type.namedWith [ "I18Next" ] "Delims" [])
            }
    , underscore =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "Underscore"
            , annotation = Just (Type.namedWith [] "Delims" [])
            }
    , custom =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "Custom"
                    , annotation = Just (Type.namedWith [] "Delims" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { delims :
        Elm.Expression
        ->
            { delimsTags_0_0
                | curly : Elm.Expression
                , underscore : Elm.Expression
                , custom : Elm.Expression -> Elm.Expression
            }
        -> Elm.Expression
    }
caseOf_ =
    { delims =
        \delimsExpression delimsTags ->
            Elm.Case.custom
                delimsExpression
                (Type.namedWith [ "I18Next" ] "Delims" [])
                [ Elm.Case.branch0 "Curly" delimsTags.curly
                , Elm.Case.branch0 "Underscore" delimsTags.underscore
                , Elm.Case.branch1
                    "Custom"
                    ( "one", Type.tuple Type.string Type.string )
                    delimsTags.custom
                ]
    }


call_ :
    { t : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tr :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , tf : Elm.Expression -> Elm.Expression -> Elm.Expression
    , trf :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , customTr :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , customTrf :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , keys : Elm.Expression -> Elm.Expression
    , hasKey : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fromTree : Elm.Expression -> Elm.Expression
    , string : Elm.Expression -> Elm.Expression
    , object : Elm.Expression -> Elm.Expression
    }
call_ =
    { t =
        \tArg tArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "t"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "I18Next" ] "Translations" []
                                , Type.string
                                ]
                                Type.string
                            )
                    }
                )
                [ tArg, tArg0 ]
    , tr =
        \trArg trArg0 trArg1 trArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "tr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "I18Next" ] "Translations" []
                                , Type.namedWith [ "I18Next" ] "Delims" []
                                , Type.string
                                , Type.namedWith [ "I18Next" ] "Replacements" []
                                ]
                                Type.string
                            )
                    }
                )
                [ trArg, trArg0, trArg1, trArg2 ]
    , tf =
        \tfArg tfArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "tf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "I18Next" ]
                                        "Translations"
                                        []
                                    )
                                , Type.string
                                ]
                                Type.string
                            )
                    }
                )
                [ tfArg, tfArg0 ]
    , trf =
        \trfArg trfArg0 trfArg1 trfArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "trf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "I18Next" ]
                                        "Translations"
                                        []
                                    )
                                , Type.namedWith [ "I18Next" ] "Delims" []
                                , Type.string
                                , Type.list (Type.tuple Type.string Type.string)
                                ]
                                Type.string
                            )
                    }
                )
                [ trfArg, trfArg0, trfArg1, trfArg2 ]
    , customTr =
        \customTrArg customTrArg0 customTrArg1 customTrArg2 customTrArg3 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "customTr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "I18Next" ] "Translations" []
                                , Type.namedWith [ "I18Next" ] "Delims" []
                                , Type.function [ Type.string ] (Type.var "a")
                                , Type.string
                                , Type.namedWith
                                    [ "I18Next" ]
                                    "CustomReplacements"
                                    [ Type.var "a" ]
                                ]
                                (Type.list (Type.var "a"))
                            )
                    }
                )
                [ customTrArg
                , customTrArg0
                , customTrArg1
                , customTrArg2
                , customTrArg3
                ]
    , customTrf =
        \customTrfArg customTrfArg0 customTrfArg1 customTrfArg2 customTrfArg3 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "customTrf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "I18Next" ]
                                        "Translations"
                                        []
                                    )
                                , Type.namedWith [ "I18Next" ] "Delims" []
                                , Type.function [ Type.string ] (Type.var "a")
                                , Type.string
                                , Type.namedWith
                                    [ "I18Next" ]
                                    "CustomReplacements"
                                    [ Type.var "a" ]
                                ]
                                (Type.list (Type.var "a"))
                            )
                    }
                )
                [ customTrfArg
                , customTrfArg0
                , customTrfArg1
                , customTrfArg2
                , customTrfArg3
                ]
    , keys =
        \keysArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "keys"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "I18Next" ] "Translations" []
                                ]
                                (Type.list Type.string)
                            )
                    }
                )
                [ keysArg ]
    , hasKey =
        \hasKeyArg hasKeyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "hasKey"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "I18Next" ] "Translations" []
                                , Type.string
                                ]
                                Type.bool
                            )
                    }
                )
                [ hasKeyArg, hasKeyArg0 ]
    , fromTree =
        \fromTreeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "fromTree"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith [ "I18Next" ] "Tree" [])
                                    )
                                ]
                                (Type.namedWith [ "I18Next" ] "Translations" [])
                            )
                    }
                )
                [ fromTreeArg ]
    , string =
        \stringArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "string"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [ "I18Next" ] "Tree" [])
                            )
                    }
                )
                [ stringArg ]
    , object =
        \objectArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "I18Next" ]
                    , name = "object"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith [ "I18Next" ] "Tree" [])
                                    )
                                ]
                                (Type.namedWith [ "I18Next" ] "Tree" [])
                            )
                    }
                )
                [ objectArg ]
    }


values_ :
    { initialTranslations : Elm.Expression
    , translationsDecoder : Elm.Expression
    , t : Elm.Expression
    , tr : Elm.Expression
    , tf : Elm.Expression
    , trf : Elm.Expression
    , customTr : Elm.Expression
    , customTrf : Elm.Expression
    , keys : Elm.Expression
    , hasKey : Elm.Expression
    , fromTree : Elm.Expression
    , string : Elm.Expression
    , object : Elm.Expression
    }
values_ =
    { initialTranslations =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "initialTranslations"
            , annotation = Just (Type.namedWith [ "I18Next" ] "Translations" [])
            }
    , translationsDecoder =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "translationsDecoder"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Json", "Decode" ]
                        "Decoder"
                        [ Type.namedWith [ "I18Next" ] "Translations" [] ]
                    )
            }
    , t =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "t"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.string
                        ]
                        Type.string
                    )
            }
    , tr =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "tr"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.string
                        , Type.namedWith [ "I18Next" ] "Replacements" []
                        ]
                        Type.string
                    )
            }
    , tf =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "tf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "I18Next" ] "Translations" [])
                        , Type.string
                        ]
                        Type.string
                    )
            }
    , trf =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "trf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "I18Next" ] "Translations" [])
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.string
                        , Type.namedWith [ "I18Next" ] "Replacements" []
                        ]
                        Type.string
                    )
            }
    , customTr =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "customTr"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.function [ Type.string ] (Type.var "a")
                        , Type.string
                        , Type.namedWith
                            [ "I18Next" ]
                            "CustomReplacements"
                            [ Type.var "a" ]
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
    , customTrf =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "customTrf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "I18Next" ] "Translations" [])
                        , Type.namedWith [ "I18Next" ] "Delims" []
                        , Type.function [ Type.string ] (Type.var "a")
                        , Type.string
                        , Type.namedWith
                            [ "I18Next" ]
                            "CustomReplacements"
                            [ Type.var "a" ]
                        ]
                        (Type.list (Type.var "a"))
                    )
            }
    , keys =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "keys"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" [] ]
                        (Type.list Type.string)
                    )
            }
    , hasKey =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "hasKey"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "I18Next" ] "Translations" []
                        , Type.string
                        ]
                        Type.bool
                    )
            }
    , fromTree =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "fromTree"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith [ "I18Next" ] "Tree" [])
                            )
                        ]
                        (Type.namedWith [ "I18Next" ] "Translations" [])
                    )
            }
    , string =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "I18Next" ] "Tree" [])
                    )
            }
    , object =
        Elm.value
            { importFrom = [ "I18Next" ]
            , name = "object"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith [ "I18Next" ] "Tree" [])
                            )
                        ]
                        (Type.namedWith [ "I18Next" ] "Tree" [])
                    )
            }
    }
