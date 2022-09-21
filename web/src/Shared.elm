module Shared exposing (..)

import I18Next exposing (Translations)
import Json.Decode as Decode exposing (Decoder)
import List.Nonempty as Nonempty exposing (Nonempty)


type LanguageId
    = EN


translationIdDecoder : Decoder LanguageId
translationIdDecoder =
    Decode.string
        |> Decode.andThen
            (\id ->
                case id of
                    "en" ->
                        Decode.succeed EN

                    _ ->
                        Decode.fail ("Unknown language id: " ++ id)
            )


type alias Shared =
    { language : List Translations
    , altLanguages : Nonempty ( LanguageId, Translations )
    }


init : Nonempty ( LanguageId, Translations ) -> Shared
init altLanguages =
    { language = []
    , altLanguages = altLanguages
    }
        |> withLanguage EN


withLanguage : LanguageId -> Shared -> Shared
withLanguage languageId shared =
    { shared
        | language =
            shared.altLanguages
                |> Nonempty.foldl
                    (\( id, language ) found ->
                        if id == languageId then
                            Just language

                        else
                            found
                    )
                    Nothing
                |> Maybe.withDefault I18Next.initialTranslations
                |> (\defaultLanguage -> defaultLanguage :: List.map Tuple.second (Nonempty.toList shared.altLanguages))
    }
