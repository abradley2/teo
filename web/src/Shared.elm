module Shared exposing
    ( ClientId(..)
    , LanguageId(..)
    , Shared
    , clientIdToHeader
    , init
    , translationIdDecoder
    )

import Http
import I18Next exposing (Translations)
import Json.Decode as Decode exposing (Decoder)
import List.Nonempty as Nonempty exposing (Nonempty)
import Theme exposing (Theme)


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
    , clientId : ClientId
    , theme : Theme
    }


type ClientId
    = ClientId String


clientIdToHeader : ClientId -> Http.Header
clientIdToHeader (ClientId clientId) =
    Http.header "X-Client-Id" clientId


init : ClientId -> Nonempty ( LanguageId, Translations ) -> Shared
init clientId altLanguages =
    { language = []
    , altLanguages = altLanguages
    , clientId = clientId
    , theme = Theme.darkTheme
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
