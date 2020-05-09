{-|
Module      : Web.Framework.Plzwrk.Asterius
Description : Asterius bindings for plzwrk
Copyright   : (c) Mike Solomon 2020
License     : GPL-3
Maintainer  : mike@meeshkan.com
Stability   : experimental
Portability : POSIX, Windows

This module exports a single function called @asteriusBrowser@
that you can use to build your DOM with asterius (see the
examples in the README.md). Unfortunately, due to the way
cabal compiles this documentation, it does not appear on
this page. Instead, a dummy function called @ignoreMe@ appears.
This is because @asteriusBrowser@ can only be created by using
`ahc-cabal`, and haddock uses `cabal` as a default.
-}
{-# LANGUAGE CPP #-}
#if defined(PLZWRK_ENABLE_ASTERIUS)
{-# LANGUAGE InterruptibleFFI  #-}
module Web.Framework.Plzwrk.Asterius (asteriusBrowser) where

import           Asterius.Aeson
import           Asterius.ByteString
import           Asterius.Types
import           Data.ByteString.Internal       ( ByteString )
import qualified Data.ByteString               as BS
import           Data.ByteString.Unsafe
import           Data.HashMap.Strict as HM
import           Data.Coerce
import           Foreign.Ptr
import           Web.Framework.Plzwrk.JSEnv

asteriusBrowser :: IO (JSEnv JSVal)
asteriusBrowser = return JSEnv
  { 
    castToArray                    = _castToArray
  , castToBool                     = _castToBool
  , castToByteString               = _castToByteString
  , castToDouble                   = _castToDouble
  , castToInt                      = _castToInt
  , castToString                   = _castToString
  , consoleLog                     = _consoleLog
  , defaultRequestInit             = _defaultRequestInit
  , documentCreateElement          = _documentCreateElement
  , documentCreateTextNode         = _documentCreateTextNode
  , documentBody                   = _documentBody
  , documentGetElementById         = _documentGetElementById
  , documentHead                   = _documentHead
  , fetch                          = _fetch
  , _freeCallback                  = __freeCallback
  , getPropertyAsOpaque            = _getPropertyAsOpaque
  , jsValFromArray                 = _jsValFromArray
  , jsValFromBool                  = _jsValFromBool
  , jsValFromByteString            = _jsValFromByteString
  , jsValFromDouble                = _jsValFromDouble
  , jsValFromInt                   = _jsValFromInt
  , jsValFromString                = _jsValFromString
  , invokeOn0                      = _invokeOn0
  , makeObject                     = _makeObject
  , setValue                       = _setValue
  , invokeOn1                      = _invokeOn1
  , invokeOn2                      = _invokeOn2
  , _makeHaskellCallback           = __makeHaskellCallback
  , mathRandom                     = _mathRandom
  }

_documentCreateElement :: String -> IO JSVal
_documentCreateElement = js_documentCreateElement . toJSString

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

_castToString :: JSVal -> IO (Maybe String)
_castToString v = pure $ (Just . fromJSString . JSString) v

_castToByteString :: JSVal -> IO (Maybe ByteString)
_castToByteString v = pure $ (Just . byteStringFromJSUint8Array . JSUint8Array) v

_castToBool :: JSVal -> IO (Maybe Bool)
_castToBool n =
  _castGeneric (\v -> (jsonFromJSVal v) :: Either String Bool) n

_castToInt :: JSVal -> IO (Maybe Int)
_castToInt n =
  _castGeneric (\v -> (jsonFromJSVal v) :: Either String Int) n

_castToDouble :: JSVal -> IO (Maybe Double)
_castToDouble n =
  _castGeneric (\v -> (jsonFromJSVal v) :: Either String Double) n

_castGeneric :: (JSVal -> Either String a) -> JSVal -> IO (Maybe a)
_castGeneric f v = do
  isUndef' <- js_null_or_undef v
  if isUndef'
    then pure Nothing
    else (let q = f v in either (\_ -> pure Nothing) (pure . Just) q)

_defaultRequestInit = RequestInit {
    _ri_method      = Nothing
  , _ri_headers     = Nothing
  , _ri_body        = Nothing
  , _ri_mode        = Nothing
  , _ri_credentials = Nothing
  , _ri_cache       = Nothing
  , _ri_redirect    = Nothing
  , _ri_referrer    = Nothing 
  , _ri_integrity   = Nothing
}

_s2v :: JSString -> JSVal
_s2v (JSString v) = v

_b2v :: JSUint8Array -> JSVal
_b2v (JSUint8Array v) = v


makeBody :: FetchBody JSVal -> IO JSVal
makeBody (FormDataBody formData) = do
  _fd <- _formData
  mapM (\(x, y) -> _invokeOn2 _fd "append" (_s2v . toJSString $ x) y) (HM.toList formData)
  return _fd
makeBody (StringBody str) = pure (_s2v . toJSString $ str)
makeBody (BlobBody blob) = pure blob
makeBody (URLSearchParamsBody searchParams) = do
  _usp <- _urlSearchParams
  mapM (\(x, y) -> _invokeOn2 _usp "append" (_s2v . toJSString $ x) (_s2v . toJSString $ y)) (HM.toList searchParams)
  return _usp
makeBody (Uint8Array bs) = pure $ (_b2v . byteStringToJSUint8Array $ bs)

kvToJSVal :: (String, String) -> IO (JSVal, JSVal)
kvToJSVal (k, v) = do
  _k <- _jsValFromString k
  _v <- _jsValFromString v
  return $ (_k, _v)

requestInitToJSVal :: RequestInit JSVal -> IO JSVal
requestInitToJSVal ri = do
  obj <- _makeObject
  maybe (pure ()) (_setValue' obj "method") (_ri_method ri)
  maybe (pure ()) (\hds -> do 
    __headers <- _headers
    kvs <- mapM kvToJSVal (HM.toList hds)
    mapM (\(x, y) -> _invokeOn2 __headers "append" x y) kvs
    _setValue obj "headers" __headers) (_ri_headers ri)
  maybe (pure ()) (\body -> do 
    _body <- makeBody body
    _setValue obj "body" _body) (_ri_body ri)
  maybe (pure ()) (_setValue' obj "mode") (_ri_mode ri)
  maybe (pure ()) (_setValue' obj "credentials") (_ri_credentials ri)
  maybe (pure ()) (_setValue' obj "cache") (_ri_cache ri)
  maybe (pure ()) (_setValue' obj "redirect") (_ri_redirect ri)
  maybe (pure ()) (_setValue' obj "referrer") (_ri_referrer ri)
  maybe (pure ()) (_setValue' obj "integrity") (_ri_integrity ri)
  return obj

_fetch :: String -> RequestInit JSVal -> IO JSVal
_fetch url ri = do
  _ri <- requestInitToJSVal ri
  _js_fetch (toJSString url) _ri

_documentCreateTextNode :: String -> IO JSVal
_documentCreateTextNode = js_documentCreateTextNode . toJSString

_invokeOn0 :: JSVal -> String -> IO JSVal
_invokeOn0 e s = _js_invokeOn0 e (toJSString s)

_invokeOn1 :: JSVal -> String -> JSVal -> IO JSVal
_invokeOn1 e s v0 = _js_invokeOn1 e (toJSString s) v0

_invokeOn2 :: JSVal -> String -> JSVal -> JSVal -> IO JSVal
_invokeOn2 e s v0 v1 = _js_invokeOn2 e (toJSString s) v0 v1

_documentGetElementById :: String -> IO (Maybe JSVal)
_documentGetElementById k = do
  v <- js_documentGetElementById (toJSString k)
  u <- js_null_or_undef v
  return $ if u then Nothing else Just v

getJSVal :: JSFunction -> JSVal
getJSVal (JSFunction x) = x

_castToArray :: JSVal -> IO (Maybe [JSVal])
_castToArray v = pure $ (Just . fromJSArray . JSArray) v

_jsValFromString :: String -> IO JSVal
_jsValFromString v = pure $ (x . toJSString) v
  where x (JSString s) = s

_jsValFromByteString :: ByteString -> IO JSVal
_jsValFromByteString v = pure $ (x . byteStringToJSUint8Array) v
  where x (JSUint8Array bs) = bs

_jsValFromArray :: [JSVal] -> IO JSVal
_jsValFromArray v = pure $ (x . toJSArray) v
  where x (JSArray bs) = bs

_setValue :: JSVal -> String -> JSVal -> IO ()
_setValue a b c = _js_setValue a (toJSString b) c

_setValue' :: JSVal -> String -> String -> IO ()
_setValue' a b c = _js_setValue a (toJSString b) $ _s2v (toJSString c)

__makeHaskellCallback :: (JSVal -> IO ()) -> IO JSVal
__makeHaskellCallback a = do
  x <- makeHaskellCallback1 a
  return $ getJSVal x

__freeCallback :: JSVal -> IO ()
__freeCallback v = freeHaskellCallback (JSFunction v)

foreign import javascript "{}"
  _makeObject :: IO JSVal

foreign import javascript "new Headers()"
  _headers :: IO JSVal

foreign import javascript "new URLSearchParams()"
  _urlSearchParams :: IO JSVal

foreign import javascript "new FormData()"
  _formData :: IO JSVal


foreign import javascript "$1[$2]=$3"
  _js_setValue :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript "$1"
  _jsValFromBool :: Bool -> IO JSVal

foreign import javascript interruptible "fetch($1, $2)"
  _js_fetch :: JSString -> JSVal -> IO JSVal

foreign import javascript "$1"
  _jsValFromInt :: Int -> IO JSVal

foreign import javascript "$1"
  _jsValFromDouble :: Double -> IO JSVal

foreign import javascript "console.log($1)"
  _consoleLog :: JSVal -> IO ()

foreign import javascript "$1[$2]()"
  _js_invokeOn0 :: JSVal -> JSString -> IO JSVal

foreign import javascript "$1[$2]($3)"
  _js_invokeOn1 :: JSVal -> JSString -> JSVal -> IO JSVal

foreign import javascript "$1[$2]($3,$4)"
  _js_invokeOn2 :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal

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

foreign import javascript "($1 == null) || ($1 == undefined)"
  js_null_or_undef :: JSVal -> IO Bool

foreign import javascript "document.createTextNode($1)"
  js_documentCreateTextNode :: JSString -> IO JSVal

foreign import javascript "document.getElementById($1)"
  js_documentGetElementById :: JSString -> IO JSVal

foreign import javascript "wrapper"
  makeHaskellCallback1 :: (JSVal -> IO ()) -> IO JSFunction

# else
module Web.Framework.Plzwrk.Asterius where

ignoreMe :: IO ()
ignoreMe = print "ignore me"
# endif