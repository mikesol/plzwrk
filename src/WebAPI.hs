{-# LANGUAGE InterruptibleFFI  #-}
{-# LANGUAGE OverloadedStrings #-}

module WebAPI
  ( consoleLog,
    createElement,
    setAttribute,
    appendChild,
    removeChild,
    insertBefore,
    setHidden,
    addEventListener,
    removeEventListener,
    createTextNode,
    replaceWith,
    windowLocationHref,
    randomDouble,
    getElementById,
    makeHaskellCallback1,
    localStorageSetItem,
    localStorageGetItem,
  )
where

import           Asterius.ByteString
import           Asterius.Types
import qualified Data.ByteString        as BS
import           Data.ByteString.Unsafe
import           Data.Coerce
import           Data.Text
import           Foreign.Ptr

toJSString_ = toJSString . unpack

createElement :: Text -> IO JSVal
createElement = js_createElement . toJSString_

setAttribute :: JSVal -> Text -> Text -> IO ()
setAttribute e k v = js_setAttribute e (toJSString_ k) (toJSString_ v)

addEventListener :: JSVal -> Text -> JSFunction -> IO ()
addEventListener target event callback =
  js_addEventListener target (toJSString_ event) callback

removeEventListener :: JSVal -> Text -> JSFunction -> IO ()
removeEventListener target event callback =
  js_removeEventListener target (toJSString_ event) callback

createTextNode :: Text -> IO JSVal
createTextNode = js_createTextNode . toJSString_

windowLocationHref :: IO String
windowLocationHref = fromJSString <$> js_window_location_href

getElementById :: Text -> IO JSVal
getElementById k = js_getElementById (toJSString_ k)

localStorageSetItem :: Text -> BS.ByteString -> IO ()
localStorageSetItem k v = do
  let ks = toJSString_ k
  vs <- unsafeUseAsCStringLen v $ uncurry js_encode
  js_localStorage_setItem ks vs
  freeJSVal (coerce ks)
  freeJSVal (coerce vs)

localStorageGetItem :: Text -> IO (Maybe BS.ByteString)
localStorageGetItem k = do
  let ks = toJSString_ k
  f <- js_localStorage_hasItem ks
  r <-
    if f
      then do
        vs <- js_localStorage_getItem ks
        buf <- js_decode vs
        let r = Just $ byteStringFromJSUint8Array buf
        freeJSVal (coerce vs)
        freeJSVal (coerce buf)
        pure r
      else pure Nothing
  freeJSVal (coerce ks)
  pure r

randomDouble :: IO Double
randomDouble = js_Math_random

foreign import javascript "console.log($1)" consoleLog :: JSVal -> IO ()

foreign import javascript "document.createElement($1)"
  js_createElement :: JSString -> IO JSVal

foreign import javascript "$1.setAttribute($2,$3)"
  js_setAttribute :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript "$1.appendChild($2)"
  appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript "$1.insertBefore($2,$3)"
  insertBefore :: JSVal -> JSVal -> JSVal -> IO ()


foreign import javascript "$1.removeChild($2)"
  removeChild :: JSVal -> JSVal -> IO ()

foreign import javascript "$1.hidden = $2"
  setHidden :: JSVal -> Bool -> IO ()

foreign import javascript "$1.addEventListener($2,$3)"
  js_addEventListener :: JSVal -> JSString -> JSFunction -> IO ()

foreign import javascript "$1.removeEventListener($2,$3)"
  js_removeEventListener :: JSVal -> JSString -> JSFunction -> IO ()

foreign import javascript "document.createTextNode($1)"
  js_createTextNode :: JSString -> IO JSVal

foreign import javascript "$1.replaceWith($2)"
  replaceWith :: JSVal -> JSVal -> IO ()

foreign import javascript "window.location.href"
  js_window_location_href :: IO JSString

foreign import javascript "Math.random()"
  js_Math_random :: IO Double

foreign import javascript "document.getElementById($1)"
  js_getElementById :: JSString -> IO JSVal

foreign import javascript "localStorage.setItem($1,$2)"
  js_localStorage_setItem :: JSString -> JSString -> IO ()

foreign import javascript "localStorage.getItem($1) !== null"
  js_localStorage_hasItem :: JSString -> IO Bool

foreign import javascript "localStorage.getItem($1)"
  js_localStorage_getItem :: JSString -> IO JSString

foreign import javascript interruptible "new Promise((resolve, reject) => { \
\  const buf = __asterius_jsffi.exposeMemory($1,$2);                        \
\  const blob = new Blob(buf);                                              \
\  const r = new FileReader();                                              \
\  r.addEventListener('load', () => { resolve(r.result); });                \
\  r.readAsDataURL(blob);                                                   \
\  })"
  js_encode :: Ptr a -> Int -> IO JSString

foreign import javascript interruptible "fetch($1).then(b => b.arrayBuffer()).then(b => new Uint8Array(b))"
  js_decode :: JSString -> IO JSUint8Array

foreign import javascript "wrapper"
  makeHaskellCallback1 :: (JSVal -> IO ()) -> IO JSFunction
