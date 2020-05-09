module Web.Framework.Plzwrk.JSEnv
  ( JSEnv(..)
  , RequestInit(..)
  , FetchBody(..)
  )
where

import           Data.ByteString.Internal  ( ByteString )
import qualified Data.HashMap.Strict      as HM

-- | A data class holding functions that operate on opaque
-- JavaScript types, parameterized by @jsval@. When possible,
-- the real names of browser functions like 'addEventListener'
-- and 'appendChild' are used.
--
-- JSEnv is currently implemented as data instead of as
-- a typeclass because it is unclear what type of additional
-- monadic transformer around IO would be most appropriate to
-- retain the state of @jsval@ objects, whereas when part of
-- a dataclass, the state retention is abstracted away.

data JSEnv jsval = JSEnv
  {
    -- | Casts a value to an array of jsvals
    castToArray                  :: jsval -> IO (Maybe [jsval])

    -- | Casts a value to a bool
  , castToBool                    :: jsval -> IO (Maybe Bool)

    -- | Casts a value to a bool
  , castToByteString              :: jsval -> IO (Maybe ByteString)

    -- | Casts a value to a double
  , castToDouble                  :: jsval -> IO (Maybe Double)

    -- | Casts a value to an int
  , castToInt                     :: jsval -> IO (Maybe Int)

    -- | Casts a value to a string
  , castToString                  :: jsval -> IO (Maybe String)

    -- | Logs an opaque JavaScript value. See [Console.log](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
  , consoleLog                     :: jsval -> IO ()

    -- | A default request init
  , defaultRequestInit             :: RequestInit jsval

    -- | The body of the document. See [Document.body](https://developer.mozilla.org/en-US/docs/Web/API/Document/body)
  , documentBody                   :: IO jsval

    -- | Creates an element with a given tag. See [Document.createElement](https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement)
  , documentCreateElement          :: String -> IO jsval

    -- | Creates a text node with the given text. See [Document.createTextNode](https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode)
  , documentCreateTextNode         :: String -> IO jsval

    -- | Gets an element by id from a document. See [Document.getElementById](https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById)
  , documentGetElementById         :: String -> IO (Maybe jsval)

    -- | Gets the head of a document. See [Document.head](https://developer.mozilla.org/en-US/docs/Web/API/Document/head)
  , documentHead                   :: IO jsval

    -- | Calls the browser's fetch command
  , fetch                          :: String -> RequestInit jsval -> IO jsval

    -- | Gets a JavaScript property, returning @Nothing@ if the object being called is null or undefined.
  , getPropertyAsOpaque            :: jsval -> String -> IO (Maybe jsval)

    -- | Invokes on target a function with 0 arguments.
  , invokeOn0                      :: jsval -> String -> IO jsval

    -- | Invokes on target a function with 1 argument.
  , invokeOn1                      :: jsval -> String -> jsval -> IO jsval

    -- | Invokes on target a function with 2 arguments.
  , invokeOn2                      :: jsval -> String -> jsval -> jsval -> IO jsval

    -- | jsval from an array of jsvals
  , jsValFromArray                 :: [jsval] -> IO jsval

    -- | jsval from a bool
  , jsValFromBool                  :: Bool -> IO jsval

    -- | jsval from a byte string
  , jsValFromByteString            :: ByteString -> IO jsval

    -- | jsval from a double
  , jsValFromDouble                :: Double -> IO jsval

    -- | jsval from an int
  , jsValFromInt                   :: Int -> IO jsval

    -- | jsval from a string
  , jsValFromString                :: String -> IO jsval

    -- | Creates an object
  , makeObject                     :: IO jsval

    -- | Generate a random double between 0 and 1. See [Math.random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
  , mathRandom                     :: IO Double

    -- | Sets on an object a value
  , setValue                       :: jsval -> String -> jsval -> IO ()

    -- | Frees a callback. Should only be called in advanced cases. In most usage, callbacks are freed automatically by plzwrk.
  , _freeCallback                  :: jsval -> IO ()

    -- | Makes a haskell callback. Should only be called in advanced cases. In most usage, callbacks are created automatically by plzwrk.
  , _makeHaskellCallback           :: (jsval -> IO ()) -> IO jsval
  }

-- |The body of a fetch request or response
data FetchBody jsval =
    FormDataBody (HM.HashMap String jsval) -- ^ From form data, ie @multipart/form-data@ or @application/x-www-form-urlencoded@
  | StringBody   String -- ^ A string, ie @text/plain@ or @application/json@
  | BlobBody     jsval -- ^ A single file
  | URLSearchParamsBody (HM.HashMap String String) -- ^ Useful for @application/x-www-form-urlencoded@
  | Uint8Array   ByteString

-- | Initialization parameters for a request made with fetch
data RequestInit jsval = RequestInit {
    _ri_method      :: Maybe String -- ^ The request method, e.g., GET, POST. The default is GET.
  , _ri_headers     :: Maybe (HM.HashMap String String) -- ^ Any headers you want to add to your request, contained within a Headers object or an object literal with ByteString values.
  , _ri_body        :: Maybe (FetchBody jsval) -- ^ Any body that you want to add to your request: this can be a Blob, BufferSource, FormData, URLSearchParams, USVString, or ReadableStream object. Note that a request using the GET or HEAD method cannot have a body.
  , _ri_mode        :: Maybe String -- ^ The mode you want to use for the request, e.g., cors, no-cors, same-origin, or navigate. The default is cors.
  , _ri_credentials :: Maybe String -- ^ The request credentials you want to use for the request: omit, same-origin, or include. The default is same-origin.
  , _ri_cache       :: Maybe String -- ^ The cache mode you want to use for the request.
  , _ri_redirect    :: Maybe String -- ^ The redirect mode to use: follow, error, or manual. The default is follow.
  , _ri_referrer    :: Maybe String -- ^ A USVString specifying no-referrer, client, or a URL. The default is about:client.
  , _ri_integrity   :: Maybe String -- ^ Contains the subresource integrity value of the request (e.g., sha256-BpfBw7ivV8q2jLiT13fxDYAe2tJllusRSZ273h2nFSE=).
}