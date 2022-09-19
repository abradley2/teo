{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Action.Cookie (readCookies, readCookie) where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding
import Handler (Handler)
import Relude
import qualified Web.Cookie as Cookie
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as ScottyT

readCookies :: ActionT LazyText.Text Handler [(Text, Text)]
readCookies =
    ScottyT.header "Cookie"
        <&> fmap Data.Text.Lazy.Encoding.encodeUtf8
        <&> fmap LazyByteString.toStrict
        <&> fmap Cookie.parseCookiesText
        <&> fromMaybe []

readCookie :: Text -> ActionT LazyText.Text Handler (Maybe Text)
readCookie cookieName =
    getCookieByName <$> readCookies
  where
    getCookieByName ((name, value) : cookies) =
        if name == cookieName then Just value else getCookieByName cookies
    getCookieByName [] = Nothing