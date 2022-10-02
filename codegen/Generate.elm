module Generate exposing (main)

{-| -}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Dict
import Json.Decode as Decode exposing (Decoder, Value)
import Parser


main : Program Value () ()
main =
    Generate.fromJson (Decode.map formatNode nodeDecoder) <|
        \translationsDict ->
            [ Elm.file
                []
                [ Elm.declaration "defaultTranslations" <|
                    Dict.foldr
                        (\k v genDict ->
                            Gen.Dict.insert (Elm.string k) (Elm.string v) genDict
                        )
                        Gen.Dict.empty
                        translationsDict
                ]
            ]


type Node
    = Translation String
    | TranslationDict (Dict String Node)


formatNode : Node -> Dict String String
formatNode node =
    let
        runNode : String -> Node -> Dict String String -> Dict String String
        runNode prefix n acc =
            case n of
                Translation str ->
                    Dict.insert prefix str acc

                TranslationDict dict ->
                    Dict.foldl
                        (\nextPrefix nextNode nextAcc ->
                            runNode
                                (if prefix == "" then
                                    nextPrefix

                                 else
                                    prefix ++ "." ++ nextPrefix
                                )
                                nextNode
                                nextAcc
                        )
                        Dict.empty
                        dict
                        |> Dict.union acc
    in
    runNode "" node Dict.empty


nodeDecoder : Decoder Node
nodeDecoder =
    Decode.oneOf
        [ Decode.map Translation Decode.string
        , Decode.map TranslationDict (Decode.dict (Decode.lazy <| \_ -> nodeDecoder))
        ]
