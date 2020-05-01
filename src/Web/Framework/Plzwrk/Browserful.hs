module Web.Framework.Plzwrk.Browserful
  ( Browserful(..)
  )
where

data Browserful jsval = Browserful
  { addEventListener    :: jsval -> String -> jsval -> IO ()
  , appendChild         :: jsval -> jsval -> IO ()
  , consoleLog          :: String -> IO ()
  , consoleLog'         :: jsval -> IO ()
  , click               :: jsval -> IO ()
  , createElement       :: String -> IO jsval
  , createTextNode      :: String -> IO jsval
  , freeCallback        :: jsval -> IO ()
  , getBody             :: IO jsval
  , getBool             :: jsval -> String -> IO (Maybe Bool)
  , getChildren         :: jsval -> IO [jsval]
  , getDouble           :: jsval -> String -> IO (Maybe Double)
  , getHead             :: IO jsval
  , getElementById      :: String -> IO (Maybe jsval)
  , getInt              :: jsval -> String -> IO (Maybe Int)
  , getOpaque           :: jsval -> String -> IO (Maybe jsval)
  , getString           :: jsval -> String -> IO (Maybe String)
  , getTag              :: jsval -> IO String
  , insertBefore        :: jsval -> jsval -> jsval -> IO ()
  , invokeOn            :: jsval -> String -> IO ()
  , makeHaskellCallback :: (jsval -> IO ()) -> IO jsval
  , random01            :: IO Double
  , removeChild         :: jsval -> jsval -> IO ()
  , removeEventListener :: jsval -> String -> jsval -> IO ()
  , setAttribute        :: jsval -> String -> String -> IO ()
  , textContent         :: jsval -> IO String
  }
