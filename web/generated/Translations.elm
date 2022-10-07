module Translations exposing (..)

{-| 
-}


import I18Next


checkAuthorizationError : List I18Next.Translations -> String
checkAuthorizationError translations =
    I18Next.tf translations "check authorization error"


