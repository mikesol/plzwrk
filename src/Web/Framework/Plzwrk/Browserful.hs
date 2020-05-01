module Web.Framework.Plzwrk.Browserful
  ( Browserful(..)
  )
where

import           Data.Text

data Browserful jsval = Browserful
  { addEventListener    :: jsval -> Text -> jsval -> IO ()
  , appendChild         :: jsval -> jsval -> IO ()
  , consoleLog          :: Text -> IO ()
  , consoleLog'         :: jsval -> IO ()
  , click               :: jsval -> IO ()
  , createElement       :: Text -> IO jsval
  , createTextNode      :: Text -> IO jsval
  , freeCallback        :: jsval -> IO ()
  , getBody             :: IO jsval
  , getBool             :: jsval -> Text -> IO (Maybe Bool)
  , getChildren         :: jsval -> IO [jsval]
  , getDouble           :: jsval -> Text -> IO (Maybe Double)
  , getHead             :: IO jsval
  , getElementById      :: Text -> IO (Maybe jsval)
  , getInt              :: jsval -> Text -> IO (Maybe Int)
  , getOpaque           :: jsval -> Text -> IO (Maybe jsval)
  , getString           :: jsval -> Text -> IO (Maybe Text)
  , getTag              :: jsval -> IO Text
  , insertBefore        :: jsval -> jsval -> jsval -> IO ()
  , invokeOn            :: jsval -> Text -> IO ()
  , makeHaskellCallback :: (jsval -> IO ()) -> IO jsval
  , random01            :: IO Double
  , removeChild         :: jsval -> jsval -> IO ()
  , removeEventListener :: jsval -> Text -> jsval -> IO ()
  , setAttribute        :: jsval -> Text -> Text -> IO ()
  , textContent         :: jsval -> IO Text
  }
