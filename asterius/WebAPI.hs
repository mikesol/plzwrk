{-# LANGUAGE InterruptibleFFI  #-}
{-# LANGUAGE OverloadedStrings #-}

module WebAPI where

import           Asterius.ByteString
import           Asterius.Types
import qualified Data.ByteString        as BS
import           Data.ByteString.Unsafe
import           Data.Coerce
import           Data.Text
import           Foreign.Ptr
import           Typeclasses

instance Browserful JSVal where
    addEventListener = _addEventListener
    appendChild = _appendChild
    createElement = _createElement
    createTextNode = _createTextNode
    freeCallback = _freeCallback
    getElementById = _getElementById
    insertBefore = _insertBefore
    makeHaskellCallback = _makeHaskellCallback
    removeChild = _removeChild
    removeEventListener = _removeEventListener
    setAttribute = _setAttribute

toJSString_ = toJSString . unpack

_createElement :: Text -> IO JSVal
_createElement = js_createElement . toJSString_

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

foreign import javascript "$1.setAttribute($2,$3)"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript "$1.appendChild($2)"
  _appendChild :: JSVal -> JSVal -> IO ()

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
