module Web.Framework.Plzwrk.Browserful
  ( Browserful(..)
  )
where

-- | A data class holding functions that operate on opaque
-- JavaScript types, parameterized by @jsval@. When possible,
-- the real names of browser functions like 'addEventListener'
-- and 'appendChild' are used.
--
-- Browserful is currently implemented as data instead of as
-- a typeclass because it is unclear what type of additional
-- monadic transformer around IO would be most appropriate to
-- retain the state of @jsval@ objects, whereas when part of
-- a dataclass, the state retention is abstracted away.

data Browserful jsval = Browserful
  {
    -- | Logs a string. See [Console.log](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
    consoleLog                     :: String -> IO ()

    -- | Logs an opaque JavaScript value. See [Console.log](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
  , consoleLog'                    :: jsval -> IO ()

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

    -- | Sets on an element an attribute. See [Element.setAttribute](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute)
  , elementSetAttribute            :: jsval -> String -> String -> IO ()

    -- | Gets the tag name of an element.  See [Element.tagName](https://developer.mozilla.org/en-US/docs/Web/API/Element/tagName)
  , elementTagName                 :: jsval -> IO String

    -- | Takes a target and an event name and adds a listener. See [EventTarget.addEventListener](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener)
  , eventTargetAddEventListener    :: jsval -> String -> jsval -> IO ()

    -- | Takes a target and an event name and removes a listener. See [EventTarget.removeEventListener](https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/removeEventListener)
  , eventTargetRemoveEventListener :: jsval -> String -> jsval -> IO ()

    -- | Gets a JavaScript property as a bool, returning @Nothing@ if the object being called is null or undefined or the property cannot be cast to a bool.
  , getPropertyAsBool              :: jsval -> String -> IO (Maybe Bool)

    -- | Gets a JavaScript property as a double, returning @Nothing@ if the object being called is null or undefined or the property cannot be cast to a double.
  , getPropertyAsDouble            :: jsval -> String -> IO (Maybe Double)

    -- | Gets a JavaScript property as an int, returning @Nothing@ if the object being called is null or undefined or the property cannot be cast to an int.
  , getPropertyAsInt               :: jsval -> String -> IO (Maybe Int)

    -- | Gets a JavaScript property, returning @Nothing@ if the object being called is null or undefined.
  , getPropertyAsOpaque            :: jsval -> String -> IO (Maybe jsval)

    -- | Gets a JavaScript property as a string, returning @Nothing@ if the object being called is null or undefined or the property cannot be cast to a string.
  , getPropertyAsString            :: jsval -> String -> IO (Maybe String)

    -- | Takes an element and clicks it. Useful for testing. See [HTMLElement.click](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/click)
  , htmlElemenetClick              :: jsval -> IO ()

    -- | Invokes on target a function with 0 arguments.
  , invokeOn0                      :: jsval -> String -> IO jsval

    -- | Generate a random double between 0 and 1. See [Math.random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
  , mathRandom                     :: IO Double

    -- | Takes a node and appends a child. See [Node.appendChild](https://developer.mozilla.org/en-US/docs/Web/API/Node/appendChild)
  , nodeAppendChild                :: jsval -> jsval -> IO ()

    -- | Get the children of a node. See [Node.childNodes](https://developer.mozilla.org/en-US/docs/Web/API/Node/childNodes)
  , nodeChildNodes                 :: jsval -> IO [jsval]

    -- | Insert into an element a new node before a reference node.  See [Node.insertBefore](https://developer.mozilla.org/en-US/docs/Web/API/Node/insertBefore)
  , nodeInsertBefore               :: jsval -> jsval -> jsval -> IO ()

    -- | From a node remove a child. See [Node.removeChild](https://developer.mozilla.org/en-US/docs/Web/API/Node/removeChild)
  , nodeRemoveChild                :: jsval -> jsval -> IO ()

    -- | Gets the text content of a node. See [Node.textContent](https://developer.mozilla.org/en-US/docs/Web/API/Node/textContent)
  , nodeTextContent                :: jsval -> IO String

    -- | Frees a callback. Should only be called in advanced cases. In most usage, callbacks are freed automatically by plzwrk.
  , _freeCallback                  :: jsval -> IO ()

    -- | Makes a haskell callback. Should only be called in advanced cases. In most usage, callbacks are created automatically by plzwrk.
  , _makeHaskellCallback           :: (jsval -> IO ()) -> IO jsval
  }
