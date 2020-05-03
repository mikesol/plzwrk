module Web.Framework.Plzwrk.MockJSVal
  ( MockJSVal(..)
  , makeMockBrowser
  , defaultInternalBrowser
  , makeMockBrowserWithContext
  )
where

import           Data.Aeson                     ( FromJSON )
import           Data.HashMap.Strict     hiding ( foldr
                                                , null
                                                )
import           Data.IORef
import           Data.List                      ( elemIndex )
import           Prelude                 hiding ( lookup )
import           System.Random
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.Browserful

data LogEvent = ListenerReceived String Int
    | AddedAsListenerTo Int
    | AttributeReceived String String
    | ChildReceived Int
    | AddedAsChildTo Int
    | RemovedNode Int
    | RemovedAsNodeFrom Int
    | RemovedListener String Int
    | RemovedAsListenerFrom Int
    | CreatedElement Int
    | CreatedTextNode Int
    | InsertedChildBefore Int Int
    | InsertedAsChildBefore Int Int
    | ElementAddedBefore Int
    | GotElementById
    | MadeCallback Int
    | FreeCallback Int
    deriving (Show, Eq)


data MockAttributes = MockAttributes
  { _d_attrs  :: HashMap String String
  , _d_events :: HashMap String MockJSVal
  }
  deriving Show

data MockJSVal = MockJSElement Int String MockAttributes [MockJSVal] [LogEvent]
    | MockJSTextNode Int String [LogEvent]
    | MockJSFunction Int (MockJSVal -> IO ()) [LogEvent]
    | MockJSObject Int (HashMap String MockJSVal) [LogEvent]
    | MockJSString Int String [LogEvent]
    | MockJSNumber Int Double [LogEvent]
    | MockJSArray Int [MockJSVal] [LogEvent]
    | MockMouseEvent Int

instance Show MockJSVal where
  show (MockJSElement a b c d e) =
    show a
      <> " "
      <> " "
      <> show b
      <> " "
      <> show c
      <> " "
      <> show d
      <> " "
      <> show e
  show (MockJSTextNode a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSFunction a _ c) = show a <> " " <> show c
  show (MockJSObject   a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSString   a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSNumber   a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSArray    a b c) = show a <> " " <> show b <> " " <> show c
  show (MockMouseEvent a    ) = show a

_withNewLog :: MockJSVal -> [LogEvent] -> MockJSVal
_withNewLog (MockJSElement a b c d _) log = MockJSElement a b c d log
_withNewLog (MockJSTextNode a b _   ) log = MockJSTextNode a b log
_withNewLog (MockJSFunction a b _   ) log = MockJSFunction a b log
_withNewLog (MockJSObject   a b _   ) log = MockJSObject a b log
_withNewLog (MockJSString   a b _   ) log = MockJSString a b log
_withNewLog (MockJSNumber   a b _   ) log = MockJSNumber a b log
_withNewLog (MockJSArray    a b _   ) log = MockJSArray a b log

_withNewAttrs :: MockJSVal -> MockAttributes -> MockJSVal
_withNewAttrs (MockJSElement n tg _ chlds log) newat =
  MockJSElement n tg newat chlds log
_withNewAttrs a _ = a

_withNewKids :: MockJSVal -> [MockJSVal] -> MockJSVal
_withNewKids (MockJSElement n tg attrs _ log) newKids =
  MockJSElement n tg attrs newKids log
_withNewKids a _ = a

_ptr :: MockJSVal -> Int
_ptr (MockJSElement a _ _ _ _) = a
_ptr (MockJSTextNode a _ _   ) = a
_ptr (MockJSFunction a _ _   ) = a
_ptr (MockJSObject   a _ _   ) = a
_ptr (MockJSString   a _ _   ) = a
_ptr (MockJSNumber   a _ _   ) = a
_ptr (MockJSArray    a _ _   ) = a

_eventTargetAddEventListener
  :: MockJSVal
  -> String
  -> MockJSVal
  -> IO (MockAttributes, [LogEvent], [LogEvent])
_eventTargetAddEventListener (MockJSElement n _ (MockAttributes atts lstns) _ logn) evt fn@(MockJSFunction m _ logm)
  = pure
    $ ( MockAttributes atts $ insert evt fn lstns
      , logn <> [ListenerReceived evt m]
      , logm <> [AddedAsListenerTo n]
      )
_eventTargetAddEventListener _ _ _ =
  error "Can only add event listener to element"

_elementSetAttribute
  :: MockJSVal -> String -> String -> IO (MockAttributes, [LogEvent])
_elementSetAttribute (MockJSElement n _ (MockAttributes atts lstns) _ logn) nm attr
  = pure
    $ ( MockAttributes (insert nm attr atts) lstns
      , logn <> [AttributeReceived nm attr]
      )
_elementSetAttribute _ _ _ = error "Can only add event listener to element"

_nodeAppendChild
  :: MockJSVal -> MockJSVal -> IO ([MockJSVal], [LogEvent], [LogEvent])
_nodeAppendChild (MockJSElement n _ _ kids logn) kid@(MockJSElement m _ _ _ logm)
  = pure
    $ (kids <> [kid], logn <> [ChildReceived m], logm <> [AddedAsChildTo n])
_nodeAppendChild (MockJSElement n _ _ kids logn) kid@(MockJSTextNode m _ logm)
  = pure
    $ (kids <> [kid], logn <> [ChildReceived m], logm <> [AddedAsChildTo n])
_nodeAppendChild _ _ = error "Can only append element to element"

__nodeRemoveChild
  :: Int
  -> [MockJSVal]
  -> [LogEvent]
  -> MockJSVal
  -> Int
  -> [LogEvent]
  -> IO ([MockJSVal], [LogEvent], [LogEvent])
__nodeRemoveChild n kids logn kid m logm = maybe
  (error ("Existing item " <> show m <> " not child of " <> show n))
  (\x ->
    pure
      $ ( take x kids <> drop (x + 1) kids
        , logn <> [RemovedNode m]
        , logm <> [RemovedAsNodeFrom n]
        )
  )
  (elemIndex (_ptr kid) (fmap _ptr kids))

_nodeRemoveChild
  :: MockJSVal -> MockJSVal -> IO ([MockJSVal], [LogEvent], [LogEvent])
_nodeRemoveChild (MockJSElement n _ _ kids logn) kid@(MockJSElement m _ _ _ logm)
  = __nodeRemoveChild n kids logn kid m logm
_nodeRemoveChild (MockJSElement n _ _ kids logn) kid@(MockJSTextNode m _ logm)
  = __nodeRemoveChild n kids logn kid m logm
_nodeRemoveChild _ _ = error "Can only remove element from element"

_eventTargetRemoveEventListener
  :: MockJSVal
  -> String
  -> MockJSVal
  -> IO (MockAttributes, [LogEvent], [LogEvent])
_eventTargetRemoveEventListener (MockJSElement n _ (MockAttributes atts lstns) _ logn) evt fn@(MockJSFunction m _ logm)
  = maybe
    (error ("Listener " <> show m <> " not child of " <> show n))
    (\x ->
      pure
        $ ( MockAttributes atts $ delete evt lstns
          , logn <> [RemovedListener evt m]
          , logm <> [RemovedAsListenerFrom n]
          )
    )
    (lookup evt lstns)
_eventTargetRemoveEventListener _ _ _ =
  error "Can only add event listener to element"

_nodeInsertBeforeInternal
  :: Int
  -> [MockJSVal]
  -> [LogEvent]
  -> MockJSVal
  -> Int
  -> [LogEvent]
  -> MockJSVal
  -> Int
  -> [LogEvent]
  -> IO
       ( [MockJSVal]
       , [LogEvent]
       , [LogEvent]
       , [LogEvent]
       )
_nodeInsertBeforeInternal n kids logn newI m logm existingI l logl = maybe
  (error ("Existing item " <> show l <> " not child of " <> show n))
  (\x ->
    pure
      $ ( take x kids <> [newI] <> drop x kids
        , logn <> [InsertedChildBefore m l]
        , logm <> [InsertedAsChildBefore n l]
        , logl <> [ElementAddedBefore m]
        )
  )
  (elemIndex (_ptr existingI) (fmap _ptr kids))


_nodeInsertBefore
  :: MockJSVal
  -> MockJSVal
  -> MockJSVal
  -> IO ([MockJSVal], [LogEvent], [LogEvent], [LogEvent])
_nodeInsertBefore (MockJSElement n _ _ kids logn) newI@(MockJSElement m _ _ _ logm) existingI@(MockJSElement l _ _ _ logl)
  = _nodeInsertBeforeInternal n kids logn newI m logm existingI l logl
_nodeInsertBefore (MockJSElement n _ _ kids logn) newI@(MockJSTextNode m _ logm) existingI@(MockJSElement l _ _ _ logl)
  = _nodeInsertBeforeInternal n kids logn newI m logm existingI l logl
_nodeInsertBefore (MockJSElement n _ _ kids logn) newI@(MockJSElement m _ _ _ logm) existingI@(MockJSTextNode l _ logl)
  = _nodeInsertBeforeInternal n kids logn newI m logm existingI l logl
_nodeInsertBefore (MockJSElement n _ _ kids logn) newI@(MockJSTextNode m _ logm) existingI@(MockJSTextNode l _ logl)
  = _nodeInsertBeforeInternal n kids logn newI m logm existingI l logl
_nodeInsertBefore _ _ _ = error "Can only append element to element"

_elementTagName :: MockJSVal -> IO String
_elementTagName (MockJSElement _ tag _ _ _) = return tag
_elementTagName _ = error "Can only get tag of element"

_nodeTextContent :: MockJSVal -> IO String
_nodeTextContent (MockJSTextNode _ txt _) = return txt
_nodeTextContent _ = error "Can only get text content of text node"


_nodeChildNodes :: MockJSVal -> IO [Int]
_nodeChildNodes (MockJSElement _ _ _ kids _) = return $ fmap _ptr kids
_nodeChildNodes _ = error "Can only get children of element"

__freeCallback :: MockJSVal -> IO [LogEvent]
__freeCallback (MockJSFunction n _ log) = pure (log <> [FreeCallback n])
__freeCallback _                        = error "Can only free function"

dummyClick :: MockJSVal -> IO ()
-- todo give real number


dummyClick (MockJSFunction _ f _) = f $ MockMouseEvent (-1)


_htmlElemenetClick :: MockJSVal -> IO ()
_htmlElemenetClick (MockJSElement _ _ (MockAttributes _ evts) _ _) = do
  let oc = lookup "click" evts
  maybe (pure ()) (\x -> dummyClick x) oc

_htmlElemenetClick _ = error "Can only free function"


--------------



data MockBrowserInternal = MockBrowserInternal
  { unBrowser :: HashMap Int MockJSVal
  , unCtr     :: Int
  }
  deriving Show

look :: IORef MockBrowserInternal -> Int -> IO MockJSVal
look env elt = do
  r <- readIORef env
  let bz = unBrowser r
  maybe (error $ "Cannot find object pointer in env: " <> (show elt))
        (\x -> return x)
        (lookup elt bz)

incr :: IORef MockBrowserInternal -> IO Int
incr env = do
  r <- readIORef env
  let ctr = unCtr r
  writeIORef env $ r { unCtr = ctr + 1 }
  return ctr


wrt :: IORef MockBrowserInternal -> Int -> MockJSVal -> IO ()
wrt env elt v = do
  r <- readIORef env
  let bz = unBrowser r
  writeIORef env $ r { unBrowser = insert elt v bz }

_'eventTargetAddEventListener
  :: IORef MockBrowserInternal -> Int -> String -> Int -> IO ()
_'eventTargetAddEventListener env elt evt fn = do
  _elt                            <- look env elt
  _fn                             <- look env fn
  (newAttrs, newLogElt, newLogFn) <- _eventTargetAddEventListener _elt evt _fn
  wrt env elt $ _withNewLog (_withNewAttrs _elt newAttrs) newLogElt
  wrt env fn $ _withNewLog _fn newLogFn

_'nodeAppendChild :: IORef MockBrowserInternal -> Int -> Int -> IO ()
_'nodeAppendChild env parent kid = do
  _parent                            <- look env parent
  _kid                               <- look env kid
  (newKids, newLogParent, newLogKid) <- _nodeAppendChild _parent _kid
  wrt env parent $ _withNewLog (_withNewKids _parent newKids) newLogParent
  wrt env kid $ _withNewLog _kid newLogKid

_'documentCreateElement :: IORef MockBrowserInternal -> String -> IO Int
_'documentCreateElement env tg = do
  i <- incr env
  let elt =
        MockJSElement i tg (MockAttributes empty empty) [] [CreatedElement i]
  wrt env i elt
  return i

_'mathRandom :: IORef MockBrowserInternal -> IO Double
_'mathRandom _ = pure 0.5

_'consoleLog :: IORef MockBrowserInternal -> String -> IO ()
_'consoleLog _ txt = print txt

_'consoleLog' :: IORef MockBrowserInternal -> Int -> IO ()
_'consoleLog' _ v = print (show v)


_'documentCreateTextNode :: IORef MockBrowserInternal -> String -> IO Int
_'documentCreateTextNode env txt = do
  i <- incr env
  let elt = MockJSTextNode i txt [CreatedTextNode i]
  wrt env i elt
  return i

_'getPropertyAsString
  :: IORef MockBrowserInternal -> Int -> String -> IO (Maybe String)
_'getPropertyAsString env _ _ = pure Nothing -- not implemented yet


_'getPropertyAsBool
  :: IORef MockBrowserInternal -> Int -> String -> IO (Maybe Bool)
_'getPropertyAsBool env _ _ = pure Nothing -- not implemented yet


_'getPropertyAsInt
  :: IORef MockBrowserInternal -> Int -> String -> IO (Maybe Int)
_'getPropertyAsInt env _ _ = pure Nothing -- not implemented yet


_'getPropertyAsDouble
  :: IORef MockBrowserInternal -> Int -> String -> IO (Maybe Double)
_'getPropertyAsDouble env _ _ = pure Nothing -- not implemented yet


_'getPropertyAsOpaque
  :: IORef MockBrowserInternal -> Int -> String -> IO (Maybe Int)
_'getPropertyAsOpaque env _ _ = pure Nothing -- not implemented yet



_'invokeOn0 :: IORef MockBrowserInternal -> Int -> String -> IO Int
_'invokeOn0 env _ _ = pure 0 -- not implemented yet



_'elementTagName :: IORef MockBrowserInternal -> Int -> IO String
_'elementTagName env elt = do
  _elt <- look env elt
  _elementTagName _elt

_'nodeChildNodes :: IORef MockBrowserInternal -> Int -> IO [Int]
_'nodeChildNodes env elt = do
  _elt <- look env elt
  _nodeChildNodes _elt

_'nodeTextContent :: IORef MockBrowserInternal -> Int -> IO String
_'nodeTextContent env elt = do
  _elt <- look env elt
  _nodeTextContent _elt

_'freeCallback :: IORef MockBrowserInternal -> Int -> IO ()
_'freeCallback env fn = do
  _fn    <- look env fn
  newLog <- __freeCallback _fn
  wrt env fn $ _withNewLog _fn newLog

_'htmlElemenetClick :: IORef MockBrowserInternal -> Int -> IO ()
_'htmlElemenetClick env elt = do
  _elt <- look env elt
  _htmlElemenetClick _elt

idEq :: String -> MockJSVal -> Bool
idEq txt (MockJSElement _ _ (MockAttributes atts _) _ _) =
  Just txt == (lookup "id" atts)
idEq _ _ = False

_'documentBody :: IORef MockBrowserInternal -> IO Int
_'documentBody ref = do
  mb <- readIORef ref
  let browser = unBrowser mb
  pt <- maybe (error "No body.") (\x -> pure $ _ptr x) $ lookup 0 browser
  return pt

_'documentHead :: IORef MockBrowserInternal -> IO Int
_'documentHead ref = pure (-1) -- need to implement in mock?


_documentGetElementByIdInternal :: MockJSVal -> String -> [Int]
_documentGetElementByIdInternal jsv@(MockJSElement _ _ _ ch _) txt =
  if (idEq txt jsv)
    then [_ptr jsv]
    else (foldr (++) [] $ fmap (\x -> _documentGetElementByIdInternal x txt) ch)
_documentGetElementByIdInternal _ _ = []

_'documentGetElementById
  :: IORef MockBrowserInternal -> String -> IO (Maybe Int)
_'documentGetElementById env txt = do
  body  <- _'documentBody env
  _body <- look env body
  let elts = _documentGetElementByIdInternal _body txt
  return $ if (null elts) then (Nothing) else (Just $ head elts)

_'nodeInsertBefore :: IORef MockBrowserInternal -> Int -> Int -> Int -> IO ()
_'nodeInsertBefore env parent newItem existingItem = do
  _parent       <- look env parent
  _newItem      <- look env newItem
  _existingItem <- look env existingItem
  (newKids, newLogParent, newLogNewItem, newLogExistingItem) <-
    _nodeInsertBefore _parent _newItem _existingItem
  wrt env parent $ _withNewLog (_withNewKids _parent newKids) newLogParent
  wrt env newItem $ _withNewLog _newItem newLogNewItem
  wrt env existingItem $ _withNewLog _existingItem newLogExistingItem

_'makeHaskellCallback :: IORef MockBrowserInternal -> (Int -> IO ()) -> IO Int
_'makeHaskellCallback env cb = do
  i <- incr env
  let elt = MockJSFunction i (\x -> cb $ _ptr x) [MadeCallback i]
  wrt env i elt
  return i

_'nodeRemoveChild :: IORef MockBrowserInternal -> Int -> Int -> IO ()
_'nodeRemoveChild env parent kid = do
  _parent                            <- look env parent
  _kid                               <- look env kid
  (newKids, newLogParent, newLogKid) <- _nodeRemoveChild _parent _kid
  wrt env parent $ _withNewLog (_withNewKids _parent newKids) newLogParent
  wrt env kid $ _withNewLog _kid newLogKid

_'eventTargetRemoveEventListener
  :: IORef MockBrowserInternal -> Int -> String -> Int -> IO ()
_'eventTargetRemoveEventListener env elt evt fn = do
  _elt                            <- look env elt
  _fn                             <- look env fn
  (newAttrs, newLogElt, newLogFn) <- _eventTargetRemoveEventListener _elt
                                                                     evt
                                                                     _fn
  wrt env elt $ _withNewLog (_withNewAttrs _elt newAttrs) newLogElt
  wrt env fn $ _withNewLog _fn newLogFn

_'elementSetAttribute
  :: IORef MockBrowserInternal -> Int -> String -> String -> IO ()
_'elementSetAttribute env elt nm attr = do
  _elt               <- look env elt
  (newAttrs, newLog) <- _elementSetAttribute _elt nm attr
  wrt env elt $ _withNewLog (_withNewAttrs _elt newAttrs) newLog

makeMockBrowserWithContext :: IORef MockBrowserInternal -> IO (Browserful Int)
makeMockBrowserWithContext r = return Browserful
  { eventTargetAddEventListener    = _'eventTargetAddEventListener r
  , nodeAppendChild                = _'nodeAppendChild r
  , consoleLog                     = _'consoleLog r
  , consoleLog'                    = _'consoleLog' r
  , htmlElemenetClick              = _'htmlElemenetClick r
  , documentCreateElement          = _'documentCreateElement r
  , documentCreateTextNode         = _'documentCreateTextNode r
  , documentBody                   = _'documentBody r
  , documentGetElementById         = _'documentGetElementById r
  , documentHead                   = _'documentHead r
  , _freeCallback                  = _'freeCallback r
  , getPropertyAsBool              = _'getPropertyAsBool r
  , getPropertyAsDouble            = _'getPropertyAsDouble r
  , getPropertyAsInt               = _'getPropertyAsInt r
  , getPropertyAsOpaque            = _'getPropertyAsOpaque r
  , getPropertyAsString            = _'getPropertyAsString r
  , elementTagName                 = _'elementTagName r
  , nodeInsertBefore               = _'nodeInsertBefore r
  , invokeOn0                      = _'invokeOn0 r
  , _makeHaskellCallback           = _'makeHaskellCallback r
  , nodeChildNodes                 = _'nodeChildNodes r
  , mathRandom                     = _'mathRandom r
  , nodeRemoveChild                = _'nodeRemoveChild r
  , eventTargetRemoveEventListener = _'eventTargetRemoveEventListener r
  , elementSetAttribute            = _'elementSetAttribute r
  , nodeTextContent                = _'nodeTextContent r
  }

defaultInternalBrowser :: IO (IORef MockBrowserInternal)
defaultInternalBrowser = do
  let body = MockJSElement 0
                           "body"
                           (MockAttributes empty empty)
                           []
                           [CreatedElement 0]
  newIORef MockBrowserInternal { unBrowser = singleton 0 body, unCtr = 1 }

makeMockBrowser :: IO (Browserful Int)
makeMockBrowser = do
  rf <- defaultInternalBrowser
  makeMockBrowserWithContext rf
