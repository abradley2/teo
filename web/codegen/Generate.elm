module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import I18NextGen
import Json.Decode exposing (Value)


main : Program Value () ()
main =
    Generate.fromJson
        I18NextGen.flagsDecoder
        I18NextGen.files

