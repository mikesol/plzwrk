{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Text               hiding ( drop
                                                , empty
                                                , foldr
                                                , length
                                                , head
                                                , null
                                                , singleton
                                                , take
                                                )
import           Prelude                 hiding ( lookup )
import           System.Random
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.Browserful

data LogEvent = ListenerReceived Text Int
    | AddedAsListenerTo Int
    | AttributeReceived Text Text
    | ChildReceived Int
    | AddedAsChildTo Int
    | RemovedNode Int
    | RemovedAsNodeFrom Int
    | RemovedListener Text Int
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
  { _d_attrs  :: HashMap Text Text
  , _d_events :: HashMap Text MockJSVal
  }
  deriving Show

data MockJSVal = MockJSElement Int Text MockAttributes [MockJSVal] [LogEvent]
    | MockJSTextNode Int Text [LogEvent]
    | MockJSFunction Int (MockJSVal -> IO ()) [LogEvent]
    | MockJSObject Int (HashMap Text MockJSVal) [LogEvent]
    | MockJSString Int Text [LogEvent]
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

_addEventListener
  :: MockJSVal
  -> Text
  -> MockJSVal
  -> IO (MockAttributes, [LogEvent], [LogEvent])
_addEventListener (MockJSElement n _ (MockAttributes atts lstns) _ logn) evt fn@(MockJSFunction m _ logm)
  = pure
    $ ( MockAttributes atts $ insert evt fn lstns
      , logn <> [ListenerReceived evt m]
      , logm <> [AddedAsListenerTo n]
      )
_addEventListener _ _ _ = error "Can only add event listener to element"

_setAttribute :: MockJSVal -> Text -> Text -> IO (MockAttributes, [LogEvent])
_setAttribute (MockJSElement n _ (MockAttributes atts lstns) _ logn) nm attr =
  pure
    $ ( MockAttributes (insert nm attr atts) lstns
      , logn <> [AttributeReceived nm attr]
      )
_setAttribute _ _ _ = error "Can only add event listener to element"

_appendChild
  :: MockJSVal -> MockJSVal -> IO ([MockJSVal], [LogEvent], [LogEvent])
_appendChild (MockJSElement n _ _ kids logn) kid@(MockJSElement m _ _ _ logm) =
  pure $ (kids <> [kid], logn <> [ChildReceived m], logm <> [AddedAsChildTo n])
_appendChild (MockJSElement n _ _ kids logn) kid@(MockJSTextNode m _ logm) =
  pure $ (kids <> [kid], logn <> [ChildReceived m], logm <> [AddedAsChildTo n])
_appendChild _ _ = error "Can only append element to element"

__removeChild
  :: Int
  -> [MockJSVal]
  -> [LogEvent]
  -> MockJSVal
  -> Int
  -> [LogEvent]
  -> IO ([MockJSVal], [LogEvent], [LogEvent])
__removeChild n kids logn kid m logm = maybe
  (error ("Existing item " <> show m <> " not child of " <> show n))
  (\x ->
    pure
      $ ( take x kids <> drop (x + 1) kids
        , logn <> [RemovedNode m]
        , logm <> [RemovedAsNodeFrom n]
        )
  )
  (elemIndex (_ptr kid) (fmap _ptr kids))

_removeChild
  :: MockJSVal -> MockJSVal -> IO ([MockJSVal], [LogEvent], [LogEvent])
_removeChild (MockJSElement n _ _ kids logn) kid@(MockJSElement m _ _ _ logm) =
  __removeChild n kids logn kid m logm
_removeChild (MockJSElement n _ _ kids logn) kid@(MockJSTextNode m _ logm) =
  __removeChild n kids logn kid m logm
_removeChild _ _ = error "Can only remove element from element"

_removeEventListener
  :: MockJSVal
  -> Text
  -> MockJSVal
  -> IO (MockAttributes, [LogEvent], [LogEvent])
_removeEventListener (MockJSElement n _ (MockAttributes atts lstns) _ logn) evt fn@(MockJSFunction m _ logm)
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
_removeEventListener _ _ _ = error "Can only add event listener to element"

_insertBeforeInternal
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
_insertBeforeInternal n kids logn newI m logm existingI l logl = maybe
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


_insertBefore
  :: MockJSVal
  -> MockJSVal
  -> MockJSVal
  -> IO ([MockJSVal], [LogEvent], [LogEvent], [LogEvent])
_insertBefore (MockJSElement n _ _ kids logn) newI@(MockJSElement m _ _ _ logm) existingI@(MockJSElement l _ _ _ logl)
  = _insertBeforeInternal n kids logn newI m logm existingI l logl
_insertBefore (MockJSElement n _ _ kids logn) newI@(MockJSTextNode m _ logm) existingI@(MockJSElement l _ _ _ logl)
  = _insertBeforeInternal n kids logn newI m logm existingI l logl
_insertBefore (MockJSElement n _ _ kids logn) newI@(MockJSElement m _ _ _ logm) existingI@(MockJSTextNode l _ logl)
  = _insertBeforeInternal n kids logn newI m logm existingI l logl
_insertBefore (MockJSElement n _ _ kids logn) newI@(MockJSTextNode m _ logm) existingI@(MockJSTextNode l _ logl)
  = _insertBeforeInternal n kids logn newI m logm existingI l logl
_insertBefore _ _ _ = error "Can only append element to element"

_getTag :: MockJSVal -> IO Text
_getTag (MockJSElement _ tag _ _ _) = return tag
_getTag _                           = error "Can only get tag of element"

_textContent :: MockJSVal -> IO Text
_textContent (MockJSTextNode _ txt _) = return txt
_textContent _ = error "Can only get text content of text node"


_getChildren :: MockJSVal -> IO [Int]
_getChildren (MockJSElement _ _ _ kids _) = return $ fmap _ptr kids
_getChildren _ = error "Can only get children of element"

_freeCallback :: MockJSVal -> IO [LogEvent]
_freeCallback (MockJSFunction n _ log) = pure (log <> [FreeCallback n])
_freeCallback _                        = error "Can only free function"

dummyClick :: MockJSVal -> IO ()
-- todo give real number

dummyClick (MockJSFunction _ f _) = f $ MockMouseEvent (-1)


_click :: MockJSVal -> IO ()
_click (MockJSElement _ _ (MockAttributes _ evts) _ _) = do
  let oc = lookup "click" evts
  maybe (pure ()) (\x -> dummyClick x) oc

_click _ = error "Can only free function"


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

_'addEventListener :: IORef MockBrowserInternal -> Int -> Text -> Int -> IO ()
_'addEventListener env elt evt fn = do
  _elt                            <- look env elt
  _fn                             <- look env fn
  (newAttrs, newLogElt, newLogFn) <- _addEventListener _elt evt _fn
  wrt env elt $ _withNewLog (_withNewAttrs _elt newAttrs) newLogElt
  wrt env fn $ _withNewLog _fn newLogFn

_'appendChild :: IORef MockBrowserInternal -> Int -> Int -> IO ()
_'appendChild env parent kid = do
  _parent                            <- look env parent
  _kid                               <- look env kid
  (newKids, newLogParent, newLogKid) <- _appendChild _parent _kid
  wrt env parent $ _withNewLog (_withNewKids _parent newKids) newLogParent
  wrt env kid $ _withNewLog _kid newLogKid

_'createElement :: IORef MockBrowserInternal -> Text -> IO Int
_'createElement env tg = do
  i <- incr env
  let elt =
        MockJSElement i tg (MockAttributes empty empty) [] [CreatedElement i]
  wrt env i elt
  return i

_'random01 :: IORef MockBrowserInternal -> IO Double
_'random01 _ = pure 0.5

_'consoleLog :: IORef MockBrowserInternal -> Text -> IO ()
_'consoleLog _ txt = print txt

_'consoleLog' :: IORef MockBrowserInternal -> Int -> IO ()
_'consoleLog' _ v = print (show v)


_'createTextNode :: IORef MockBrowserInternal -> Text -> IO Int
_'createTextNode env txt = do
  i <- incr env
  let elt = MockJSTextNode i txt [CreatedTextNode i]
  wrt env i elt
  return i

_'getString :: IORef MockBrowserInternal -> Int -> Text -> IO (Maybe Text)
_'getString env _ _ = pure Nothing -- not implemented yet

_'getBool :: IORef MockBrowserInternal -> Int -> Text -> IO (Maybe Bool)
_'getBool env _ _ = pure Nothing -- not implemented yet

_'getInt :: IORef MockBrowserInternal -> Int -> Text -> IO (Maybe Int)
_'getInt env _ _ = pure Nothing -- not implemented yet

_'getDouble :: IORef MockBrowserInternal -> Int -> Text -> IO (Maybe Double)
_'getDouble env _ _ = pure Nothing -- not implemented yet

_'getOpaque :: IORef MockBrowserInternal -> Int -> Text -> IO (Maybe Int)
_'getOpaque env _ _ = pure Nothing -- not implemented yet


_'invokeOn :: IORef MockBrowserInternal -> Int -> Text -> IO ()
_'invokeOn env _ _ = pure () -- not implemented yet


_'getTag :: IORef MockBrowserInternal -> Int -> IO Text
_'getTag env elt = do
  _elt <- look env elt
  _getTag _elt

_'getChildren :: IORef MockBrowserInternal -> Int -> IO [Int]
_'getChildren env elt = do
  _elt <- look env elt
  _getChildren _elt

_'textContent :: IORef MockBrowserInternal -> Int -> IO Text
_'textContent env elt = do
  _elt <- look env elt
  _textContent _elt

_'freeCallback :: IORef MockBrowserInternal -> Int -> IO ()
_'freeCallback env fn = do
  _fn    <- look env fn
  newLog <- _freeCallback _fn
  wrt env fn $ _withNewLog _fn newLog

_'click :: IORef MockBrowserInternal -> Int -> IO ()
_'click env elt = do
  _elt <- look env elt
  _click _elt

idEq :: Text -> MockJSVal -> Bool
idEq txt (MockJSElement _ _ (MockAttributes atts _) _ _) =
  Just txt == (lookup "id" atts)
idEq _ _ = False

_'getBody :: IORef MockBrowserInternal -> IO Int
_'getBody ref = do
  mb <- readIORef ref
  let browser = unBrowser mb
  pt <- maybe (error "No body.") (\x -> pure $ _ptr x) $ lookup 0 browser
  return pt

_'getHead :: IORef MockBrowserInternal -> IO Int
_'getHead ref = pure (-1) -- need to implement in mock?

_getElementByIdInternal :: MockJSVal -> Text -> [Int]
_getElementByIdInternal jsv@(MockJSElement _ _ _ ch _) txt = if (idEq txt jsv)
  then [_ptr jsv]
  else (foldr (++) [] $ fmap (\x -> _getElementByIdInternal x txt) ch)
_getElementByIdInternal _ _ = []

_'getElementById :: IORef MockBrowserInternal -> Text -> IO (Maybe Int)
_'getElementById env txt = do
  body  <- _'getBody env
  _body <- look env body
  let elts = _getElementByIdInternal _body txt
  return $ if (null elts) then (Nothing) else (Just $ head elts)

_'insertBefore :: IORef MockBrowserInternal -> Int -> Int -> Int -> IO ()
_'insertBefore env parent newItem existingItem = do
  _parent       <- look env parent
  _newItem      <- look env newItem
  _existingItem <- look env existingItem
  (newKids, newLogParent, newLogNewItem, newLogExistingItem) <- _insertBefore
    _parent
    _newItem
    _existingItem
  wrt env parent $ _withNewLog (_withNewKids _parent newKids) newLogParent
  wrt env newItem $ _withNewLog _newItem newLogNewItem
  wrt env existingItem $ _withNewLog _existingItem newLogExistingItem

_'makeHaskellCallback :: IORef MockBrowserInternal -> (Int -> IO ()) -> IO Int
_'makeHaskellCallback env cb = do
  i <- incr env
  let elt = MockJSFunction i (\x -> cb $ _ptr x) [MadeCallback i]
  wrt env i elt
  return i

_'removeChild :: IORef MockBrowserInternal -> Int -> Int -> IO ()
_'removeChild env parent kid = do
  _parent                            <- look env parent
  _kid                               <- look env kid
  (newKids, newLogParent, newLogKid) <- _removeChild _parent _kid
  wrt env parent $ _withNewLog (_withNewKids _parent newKids) newLogParent
  wrt env kid $ _withNewLog _kid newLogKid

_'removeEventListener
  :: IORef MockBrowserInternal -> Int -> Text -> Int -> IO ()
_'removeEventListener env elt evt fn = do
  _elt                            <- look env elt
  _fn                             <- look env fn
  (newAttrs, newLogElt, newLogFn) <- _removeEventListener _elt evt _fn
  wrt env elt $ _withNewLog (_withNewAttrs _elt newAttrs) newLogElt
  wrt env fn $ _withNewLog _fn newLogFn

_'setAttribute :: IORef MockBrowserInternal -> Int -> Text -> Text -> IO ()
_'setAttribute env elt nm attr = do
  _elt               <- look env elt
  (newAttrs, newLog) <- _setAttribute _elt nm attr
  wrt env elt $ _withNewLog (_withNewAttrs _elt newAttrs) newLog

makeMockBrowserWithContext :: IORef MockBrowserInternal -> IO (Browserful Int)
makeMockBrowserWithContext r = return Browserful
  { addEventListener    = _'addEventListener r
  , appendChild         = _'appendChild r
  , consoleLog          = _'consoleLog r
  , consoleLog'         = _'consoleLog' r
  , click               = _'click r
  , createElement       = _'createElement r
  , createTextNode      = _'createTextNode r
  , freeCallback        = _'freeCallback r
  , getBody             = _'getBody r
  , getBool             = _'getBool r
  , getChildren         = _'getChildren r
  , getDouble           = _'getDouble r
  , getElementById      = _'getElementById r
  , getHead             = _'getHead r
  , getInt              = _'getInt r
  , getOpaque           = _'getOpaque r
  , getString           = _'getString r
  , getTag              = _'getTag r
  , insertBefore        = _'insertBefore r
  , invokeOn            = _'invokeOn r
  , makeHaskellCallback = _'makeHaskellCallback r
  , random01            = _'random01 r
  , removeChild         = _'removeChild r
  , removeEventListener = _'removeEventListener r
  , setAttribute        = _'setAttribute r
  , textContent         = _'textContent r
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
