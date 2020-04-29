module Web.Framework.Plzwrk.Browserful
  ( Browserful(..)
  )
where

import           Data.Text

data Browserful jsval = Browserful
  { addEventListener    :: jsval -> Text -> jsval -> IO ()
  , appendChild         :: jsval -> jsval -> IO ()
  , click               :: jsval -> IO ()
  , createElement       :: Text -> IO jsval
  , createTextNode      :: Text -> IO jsval
  , freeCallback        :: jsval -> IO ()
  , getBody             :: IO jsval
  , getChildren         :: jsval -> IO [jsval]
  , getElementById      :: Text -> IO (Maybe jsval)
  , getTag              :: jsval -> IO Text
  , insertBefore        :: jsval -> jsval -> jsval -> IO ()
  , makeHaskellCallback :: (jsval -> IO ()) -> IO jsval
  , removeChild         :: jsval -> jsval -> IO ()
  , removeEventListener :: jsval -> Text -> jsval -> IO ()
  , setAttribute        :: jsval -> Text -> Text -> IO ()
  , textContent         :: jsval -> IO Text
  }
