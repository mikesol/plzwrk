{-# LANGUAGE CPP #-}
#if defined(PLZWRK_ENABLE_ASTERIUS)
{-# LANGUAGE InterruptibleFFI  #-}
module Web.Framework.Plzwrk.Asterius (asteriusBrowser) where

import           Asterius.Aeson
import           Asterius.ByteString
import           Asterius.Types
import qualified Data.ByteString               as BS
import           Data.ByteString.Unsafe
import           Data.Coerce
import           Foreign.Ptr
import           Web.Framework.Plzwrk.Browserful

asteriusBrowser :: IO (Browserful JSVal)
asteriusBrowser = return Browserful
  { eventTargetAddEventListener    = _eventTargetAddEventListener
  , nodeAppendChild                = _nodeAppendChild
  , htmlElemenetClick              = _htmlElemenetClick
  , consoleLog                     = _consoleLog
  , consoleLog'                    = _consoleLog'
  , documentCreateElement          = _documentCreateElement
  , documentCreateTextNode         = _documentCreateTextNode
  , documentBody                   = _documentBody
  , documentGetElementById         = _documentGetElementById
  , documentHead                   = _documentHead
  , _freeCallback                  = __freeCallback
  , getPropertyAsBool              = _getPropertyAsBool
  , getPropertyAsDouble            = _getPropertyAsDouble
  , getPropertyAsInt               = _getPropertyAsInt
  , getPropertyAsOpaque            = _getPropertyAsOpaque
  , getPropertyAsString            = _getString
  , elementTagName                 = _elementTagName
  , nodeInsertBefore               = _nodeInsertBefore
  , invokeOn0                      = _invokeOn0
  , _makeHaskellCallback           = __makeHaskellCallback
  , nodeChildNodes                 = _nodeChildNodes
  , mathRandom                     = _mathRandom
  , nodeRemoveChild                = _nodeRemoveChild
  , eventTargetRemoveEventListener = _eventTargetRemoveEventListener
  , elementSetAttribute            = _elementSetAttribute
  , nodeTextContent                = _nodeTextContent
  }

_documentCreateElement :: String -> IO JSVal
_documentCreateElement = js_documentCreateElement . toJSString

_elementTagName :: JSVal -> IO String
_elementTagName x = do
  v <- js_elementTagName x
  return $ fromJSString v

_nodeTextContent :: JSVal -> IO String
_nodeTextContent x = do
  v <- js_nodeTextContent x
  return $ fromJSString v

_elementSetAttribute :: JSVal -> String -> String -> IO ()
_elementSetAttribute e k v =
  js_elementSetAttribute e (toJSString k) (toJSString v)

_getPropertyAsOpaque :: JSVal -> String -> IO (Maybe JSVal)
_getPropertyAsOpaque n k = do
  isUndef <- js_null_or_undef n
  if isUndef
    then pure Nothing
    else
      (do
        v        <- _js_getPropertyAsOpaque n (toJSString k)
        isUndef' <- js_null_or_undef v
        if isUndef' then pure Nothing else pure (Just v)
      )

_getString :: JSVal -> String -> IO (Maybe String)
_getString n k =
  _getGeneric (\v -> (jsonFromJSVal v) :: Either String String) n k

_getPropertyAsBool :: JSVal -> String -> IO (Maybe Bool)
_getPropertyAsBool n k =
  _getGeneric (\v -> (jsonFromJSVal v) :: Either String Bool) n k

_getPropertyAsInt :: JSVal -> String -> IO (Maybe Int)
_getPropertyAsInt n k =
  _getGeneric (\v -> (jsonFromJSVal v) :: Either String Int) n k

_getPropertyAsDouble :: JSVal -> String -> IO (Maybe Double)
_getPropertyAsDouble n k =
  _getGeneric (\v -> (jsonFromJSVal v) :: Either String Double) n k

_getGeneric :: (JSVal -> Either String a) -> JSVal -> String -> IO (Maybe a)
_getGeneric f n k = do
  isUndef <- js_null_or_undef n
  if isUndef
    then pure Nothing
    else
      (do
        v        <- _js_getPropertyAsOpaque n (toJSString k)
        isUndef' <- js_null_or_undef v
        if isUndef'
          then pure Nothing
          else (let q = f v in either (\_ -> pure Nothing) (pure . Just) q)
      )

_consoleLog :: String -> IO ()
_consoleLog t = _js_consoleLog (toJSString t)

_consoleLog' :: JSVal -> IO ()
_consoleLog' v = _js_consoleLog' v


_eventTargetAddEventListener :: JSVal -> String -> JSVal -> IO ()
_eventTargetAddEventListener target event callback =
  js_eventTargetAddEventListener target (toJSString event) callback

_eventTargetRemoveEventListener :: JSVal -> String -> JSVal -> IO ()
_eventTargetRemoveEventListener target event callback =
  js_eventTargetRemoveEventListener target (toJSString event) callback

_documentCreateTextNode :: String -> IO JSVal
_documentCreateTextNode = js_documentCreateTextNode . toJSString

_invokeOn0 :: JSVal -> String -> IO JSVal
_invokeOn0 e s = _js_invokeOn0 e (toJSString s)

_documentGetElementById :: String -> IO (Maybe JSVal)
_documentGetElementById k = do
  v <- js_documentGetElementById (toJSString k)
  u <- js_null_or_undef v
  return $ if u then Nothing else Just v

getJSVal :: JSFunction -> JSVal
getJSVal (JSFunction x) = x

__makeHaskellCallback :: (JSVal -> IO ()) -> IO JSVal
__makeHaskellCallback a = do
  x <- makeHaskellCallback1 a
  return $ getJSVal x

__freeCallback :: JSVal -> IO ()
__freeCallback v = freeHaskellCallback (JSFunction v)

_nodeChildNodes :: JSVal -> IO [JSVal]
_nodeChildNodes x = do
  v <- _js_nodeChildNodes x
  return $ fromJSArray v

foreign import javascript "console.log($1)"
  _js_consoleLog :: JSString -> IO ()

foreign import javascript "console.log($1)"
  _js_consoleLog' :: JSVal -> IO ()

foreign import javascript "$1[$2]()"
  _js_invokeOn0 :: JSVal -> JSString -> IO JSVal

foreign import javascript "$1[$2]"
  _js_getPropertyAsOpaque :: JSVal -> JSString -> IO JSVal

foreign import javascript "document.createElement($1)"
  js_documentCreateElement :: JSString -> IO JSVal

foreign import javascript "Math.random()"
  _mathRandom :: IO Double

foreign import javascript "document.body"
  _documentBody :: IO JSVal

foreign import javascript "document.head"
  _documentHead :: IO JSVal

foreign import javascript "$1.tagName"
  js_elementTagName :: JSVal -> IO JSString

foreign import javascript "$1.textContent"
  js_nodeTextContent :: JSVal -> IO JSString

foreign import javascript "$1.setAttribute($2,$3)"
  js_elementSetAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript "$1.appendChild($2)"
  _nodeAppendChild :: JSVal -> JSVal -> IO ()

foreign import javascript "($1 == null) || ($1 == undefined)"
  js_null_or_undef :: JSVal -> IO Bool

foreign import javascript "$1.childNodes"
  _js_nodeChildNodes :: JSVal -> IO JSArray

foreign import javascript "$1.click()"
  _htmlElemenetClick :: JSVal -> IO ()

foreign import javascript "$1.insertBefore($2,$3)"
  _nodeInsertBefore :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "$1.removeChild($2)"
  _nodeRemoveChild :: JSVal -> JSVal -> IO ()

foreign import javascript "$1.addEventListener($2,$3)"
  js_eventTargetAddEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "$1.removeEventListener($2,$3)"
  js_eventTargetRemoveEventListener :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "document.createTextNode($1)"
  js_documentCreateTextNode :: JSString -> IO JSVal

foreign import javascript "document.getElementById($1)"
  js_documentGetElementById :: JSString -> IO JSVal

foreign import javascript "wrapper oneshot"
  makeHaskellCallback1 :: (JSVal -> IO ()) -> IO JSFunction

# else
module Web.Framework.Plzwrk.Asterius where

ignoreMe :: IO ()
ignoreMe = print "ignore me"
# endif