module Routes exposing (..)

import Url
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Dashboard
    | Login
    | NotFound


routeParser : Parser (Route -> msg) msg
routeParser =
    Parser.oneOf
        [ Parser.map Dashboard (Parser.s "home")
        , Parser.map Login (Parser.s "login")
        ]


parseUrl : String -> Route
parseUrl =
    Url.fromString >> Maybe.andThen (Parser.parse routeParser) >> Maybe.withDefault NotFound
