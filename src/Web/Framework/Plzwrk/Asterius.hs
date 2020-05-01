{-# LANGUAGE CPP #-}
#if defined(PLZWRK_ENABLE_ASTERIUS)
{-# LANGUAGE InterruptibleFFI  #-}
module Web.Framework.Plzwrk.Asterius (asteriusBrowser) where

import Asterius.Aeson
import           Asterius.ByteString
import           Asterius.Types
import qualified Data.ByteString        as BS
import           Data.ByteString.Unsafe
import           Data.Coerce
import           Foreign.Ptr
import           Web.Framework.Plzwrk.Browserful

asteriusBrowser :: IO (Browserful JSVal)
asteriusBrowser = return Browserful
  { addEventListener    = _addEventListener
  , appendChild         = _appendChild
  , click               = _click
  , consoleLog          = _consoleLog
  , consoleLog'         = _consoleLog'
  , createElement       = _createElement
  , createTextNode      = _createTextNode
  , freeCallback        = _freeCallback
  , getBody             = _getBody
  , getBool             = _getBool
  , getDouble           = _getDouble
  , getChildren         = _getChildren
  , getElementById      = _getElementById
  , getHead             = _getHead
  , getInt              = _getInt
  , getOpaque           = _getOpaque
  , getString           = _getString
  , getTag              = _getTag
  , insertBefore        = _insertBefore
  , invokeOn            = _invokeOn
  , makeHaskellCallback = _makeHaskellCallback
  , random01            = _random01
  , removeChild         = _removeChild
  , removeEventListener = _removeEventListener
  , setAttribute        = _setAttribute
  , textContent         = _textContent
  }

_createElement :: String -> IO JSVal
_createElement = js_createElement . toJSString

_getTag :: JSVal -> IO String
_getTag x =  do
  v <- js_getTag x
  return $ fromJSString v

_textContent :: JSVal -> IO String
_textContent x =  do
  v <- js_textContent x
  return $ fromJSString v

_setAttribute :: JSVal -> String -> String -> IO ()
_setAttribute e k v = js_setAttribute e (toJSString k) (toJSString v)

_getOpaque :: JSVal -> String -> IO (Maybe JSVal)
_getOpaque n k = do
  isUndef <- js_null_or_undef n
  if isUndef then pure Nothing else (do
      v <- _js_getOpaque n (toJSString k)
      isUndef' <- js_null_or_undef v
      if isUndef' then pure Nothing else pure (Just v)
    )

_getString :: JSVal -> String -> IO (Maybe String)
_getString n k = _getGeneric (\v -> (jsonFromJSVal v) :: Either String String) n k

_getBool :: JSVal -> String -> IO (Maybe Bool)
_getBool n k = _getGeneric (\v -> (jsonFromJSVal v) :: Either String Bool) n k

_getInt :: JSVal -> String -> IO (Maybe Int)
_getInt n k = _getGeneric (\v -> (jsonFromJSVal v) :: Either String Int) n k

_getDouble :: JSVal -> String -> IO (Maybe Double)
_getDouble n k = _getGeneric (\v -> (jsonFromJSVal v) :: Either String Double) n k

_getGeneric :: (JSVal -> Either String a) -> JSVal -> String -> IO (Maybe a)
_getGeneric f n k = do
  isUndef <- js_null_or_undef n
  if isUndef then pure Nothing else (do
    v <- _js_getOpaque n (toJSString k)
    isUndef' <- js_null_or_undef v
    if isUndef' then pure Nothing else (
        let q = f v in
        either (\_ -> pure Nothing) (pure . Just) q)
      )

_consoleLog :: String -> IO ()
_consoleLog t = _js_consoleLog (toJSString t)

_consoleLog' :: JSVal -> IO ()
_consoleLog' v = _js_consoleLog' v


_addEventListener :: JSVal -> String -> JSVal -> IO ()
_addEventListener target event callback =
  js_addEventListener target (toJSString event) callback

_removeEventListener :: JSVal -> String -> JSVal -> IO ()
_removeEventListener target event callback =
  js_removeEventListener target (toJSString event) callback

_createTextNode :: String -> IO JSVal
_createTextNode = js_createTextNode . toJSString

_invokeOn :: JSVal -> String -> IO ()
_invokeOn e s = _js_invokeOn e (toJSString s)


_getElementById :: String -> IO (Maybe JSVal)
_getElementById k = do
  v <- js_getElementById (toJSString k)
  u <- js_null_or_undef v
  return $ if u then Nothing else Just v

getJSVal :: JSFunction -> JSVal
getJSVal (JSFunction x) = x

_makeHaskellCallback :: (JSVal -> IO ()) -> IO JSVal
_makeHaskellCallback a = do
  x <- makeHaskellCallback1 a
  return $ getJSVal x

_freeCallback :: JSVal -> IO ()
_freeCallback v = freeHaskellCallback (JSFunction v)

foreign import javascript "console.log($1)"
  _js_consoleLog :: JSString -> IO ()

foreign import javascript "console.log($1)"
  _js_consoleLog' :: JSVal -> IO ()


foreign import javascript "$1[$2]()"
  _js_invokeOn :: JSVal -> JSString -> IO ()

foreign import javascript "$1[$2]"
  _js_getOpaque :: JSVal -> JSString -> IO JSVal

foreign import javascript "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal

foreign import javascript "Math.random()"
  _random01 :: IO Double

foreign import javascript "document.body"
  _getBody :: IO JSVal

foreign import javascript "document.head"
  _getHead :: IO JSVal

foreign import javascript "$1.tagName"
  js_getTag :: JSVal -> IO JSString

foreign import javascript "$1.textContent"
  js_textContent :: JSVal -> IO JSString

foreign import javascript "$1.setAttribute($2,$3)"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript "$1.appendChild($2)"
  _appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript "($1 == null) || ($1 == undefined)"
  js_null_or_undef :: JSVal -> IO Bool

foreign import javascript "$1.childNodes"
  _js_getChildren :: JSVal -> IO JSArray

_getChildren :: JSVal -> IO [JSVal]
_getChildren x = do
  v <- _js_getChildren x
  return $ fromJSArray v

foreign import javascript "$1.click()"
  _click :: JSVal -> IO ()

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

foreign import javascript "wrapper oneshot"
  makeHaskellCallback1 :: (JSVal -> IO ()) -> IO JSFunction

# else
module Web.Framework.Plzwrk.Asterius where

ignoreMe :: IO ()
ignoreMe = print "ignore me"
# endif