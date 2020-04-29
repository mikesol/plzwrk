{-# LANGUAGE CPP #-}
#if defined(PLZWRK_ENABLE_ASTERIUS)
{-# LANGUAGE InterruptibleFFI  #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Framework.Plzwrk.Asterius (asteriusBrowser) where

import           Asterius.ByteString
import           Asterius.Types
import qualified Data.ByteString        as BS
import           Data.ByteString.Unsafe
import           Data.Coerce
import           Data.Text
import           Foreign.Ptr
import           Typeclasses

asteriusBrowser :: IO (Browserful JSVal)
asteriusBrowser = return Browserful
  { addEventListener    = _addEventListener r
  , appendChild         = _appendChild r
  , click               = _click r
  , createElement       = _createElement r
  , createTextNode      = _createTextNode r
  , freeCallback        = _freeCallback r
  , getBody             = _getBody r
  , getChildren         = _getChildren r
  , getElementById      = _getElementById r
  , getTag              = _getTag r
  , insertBefore        = _insertBefore r
  , makeHaskellCallback = _makeHaskellCallback r
  , removeChild         = _removeChild r
  , removeEventListener = _removeEventListener r
  , setAttribute        = _setAttribute r
  , textContent         = _textContent r
  }

toJSString_ = toJSString . unpack
fromJSString_ = pack . fromJSString

_createElement :: Text -> IO JSVal
_createElement = js_createElement . toJSString_

_getTag :: JSVal -> IO Text
_getTag =  fromJSString_ . js_getTag

_textContent :: JSVal -> IO Text
_textContent =  fromJSString_ . js_textContent

_setAttribute :: JSVal -> Text -> Text -> IO ()
_setAttribute e k v = js_setAttribute e (toJSString_ k) (toJSString_ v)

_addEventListener :: JSVal -> Text -> JSVal -> IO ()
_addEventListener target event callback =
  js_addEventListener target (toJSString_ event) callback

_removeEventListener :: JSVal -> Text -> JSVal -> IO ()
_removeEventListener target event callback =
  js_removeEventListener target (toJSString_ event) callback

_createTextNode :: Text -> IO JSVal
_createTextNode = js_createTextNode . toJSString_

_getElementById :: Text -> IO JSVal
_getElementById k = js_getElementById (toJSString_ k)

getJSVal :: JSFunction -> JSVal
getJSVal (JSFunction x) = x

_makeHaskellCallback :: (JSVal -> IO ()) -> IO JSVal
_makeHaskellCallback a = do
  x <- makeHaskellCallback1 a
  return $ getJSVal x

_freeCallback :: JSVal -> IO ()
_freeCallback v = freeHaskellCallback (JSFunction v)

foreign import javascript "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal

foreign import javascript "document.body"
  _getBody :: IO JSVal

foreign import javascript "$1.tagName"
  js_getTag :: JSVal -> IO JSString

foreign import javascript "$1.textContent"
  js_textContent :: JSVal -> IO JSString

foreign import javascript "$1.setAttribute($2,$3)"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript "$1.appendChild($2)"
  _appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript "$1.childNodes"
  _getChildren :: JSVal -> IO ()

foreign import javascript "$1.insertBefore($2,$3)"
  _insertBefore :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "$1.removeChild($2)"
  _removeChild :: JSVal -> JSVal -> IO ()

foreign import javascript "$1.addEventListener($2,$3)"
  js_addEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "$1.removeEventListener($2,$3)"
  js_removeEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO JSVal

foreign import javascript "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript "wrapper"
  makeHaskellCallback1 :: (JSVal -> IO ()) -> IO JSFunction

# else
module Web.Framework.Plzwrk.Asterius where

ignoreMe :: IO ()
ignoreMe = print "ignore me"
# endif