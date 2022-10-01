module Action.Cookie (readCookies, readCookie, setCookie) where

import Action (ActionM)
import Data.ByteString.Builder qualified
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyText
import Relude
import Web.Cookie qualified as Cookie
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

readCookies :: ActionT LazyText.Text ActionM [(Text, Text)]
readCookies =
    ScottyT.header "Cookie"
        <&> fmap LazyText.encodeUtf8
        <&> fmap LazyByteString.toStrict
        <&> fmap Cookie.parseCookiesText
        <&> fromMaybe []

readCookie :: Text -> ActionT LazyText.Text ActionM (Maybe Text)
readCookie cookieName =
    getCookieByName <$> readCookies
  where
    getCookieByName ((name, value) : cookies) =
        if name == cookieName then Just value else getCookieByName cookies
    getCookieByName [] = Nothing

setCookie :: Text -> Text -> ActionT LazyText.Text ActionM ()
setCookie name value =
    ScottyT.setHeader "Set-Cookie" $
        LazyText.decodeUtf8 $
            Data.ByteString.Builder.toLazyByteString $
                Cookie.renderSetCookie $
                    Cookie.defaultSetCookie
                        { Cookie.setCookieName = Text.encodeUtf8 name
                        , Cookie.setCookieValue = Text.encodeUtf8 value
                        , Cookie.setCookieHttpOnly = True
                        , Cookie.setCookieSecure = True
                        }