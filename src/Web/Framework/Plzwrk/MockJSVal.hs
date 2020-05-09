{-|

Module      : Web.Framework.Plzwrk.MockJSVal

Description : Mock browser for testing

Copyright   : (c) Mike Solomon 2020

License     : GPL-3

Maintainer  : mike@meeshkan.com

Stability   : experimental

Portability : POSIX, Windows



This module exports a mock browser called

@defaultInternalBrowser@ used in plzwrk's tests

and that can be used in your unit tests as well.

-}
module Web.Framework.Plzwrk.MockJSVal
  ( MockJSVal(..)
  , makeMockBrowser
  , defaultInternalBrowser
  , makeMockBrowserWithContext
  )
where

import           Data.Aeson                     ( FromJSON )
import           Data.ByteString.Internal       ( ByteString )
import           Data.HashMap.Strict     hiding ( foldr
                                                , null
                                                )
import           Data.IORef
import           Data.List                      ( elemIndex )
import           Prelude                 hiding ( lookup )
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
    | MockJSObject Int (HashMap String Int) [LogEvent]
    | MockJSString Int String [LogEvent]
    | MockJSDouble Int Double [LogEvent]
    | MockJSInt Int Int [LogEvent]
    | MockJSBool Int Bool [LogEvent]
    | MockJSByteString Int ByteString [LogEvent]
    | MockJSArray Int [Int] [LogEvent]
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
  show (MockJSTextNode   a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSFunction   a _ c) = show a <> " " <> show c
  show (MockJSObject     a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSString     a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSDouble     a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSInt        a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSBool       a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSByteString a b c) = show a <> " " <> show b <> " " <> show c
  show (MockJSArray      a b c) = show a <> " " <> show b <> " " <> show c
  show (MockMouseEvent a      ) = show a

_withNewLog :: MockJSVal -> [LogEvent] -> MockJSVal
_withNewLog (MockJSElement a b c d _) log = MockJSElement a b c d log
_withNewLog (MockJSTextNode   a b _ ) log = MockJSTextNode a b log
_withNewLog (MockJSFunction   a b _ ) log = MockJSFunction a b log
_withNewLog (MockJSObject     a b _ ) log = MockJSObject a b log
_withNewLog (MockJSString     a b _ ) log = MockJSString a b log
_withNewLog (MockJSDouble     a b _ ) log = MockJSDouble a b log
_withNewLog (MockJSBool       a b _ ) log = MockJSBool a b log
_withNewLog (MockJSInt        a b _ ) log = MockJSInt a b log
_withNewLog (MockJSByteString a b _ ) log = MockJSByteString a b log
_withNewLog (MockJSArray      a b _ ) log = MockJSArray a b log

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
_ptr (MockJSTextNode   a _ _ ) = a
_ptr (MockJSFunction   a _ _ ) = a
_ptr (MockJSObject     a _ _ ) = a
_ptr (MockJSString     a _ _ ) = a
_ptr (MockJSDouble     a _ _ ) = a
_ptr (MockJSBool       a _ _ ) = a
_ptr (MockJSInt        a _ _ ) = a
_ptr (MockJSByteString a _ _ ) = a
_ptr (MockJSArray      a _ _ ) = a

_eventTargetAddEventListener
  :: MockJSVal
  -> String
  -> MockJSVal
  -> IO (MockAttributes, [LogEvent], [LogEvent])
_eventTargetAddEventListener (MockJSElement n _ (MockAttributes atts lstns) _ logn) evt fn@(MockJSFunction m _ logm)
  = pure
    ( MockAttributes atts $ insert evt fn lstns
    , logn <> [ListenerReceived evt m]
    , logm <> [AddedAsListenerTo n]
    )
_eventTargetAddEventListener _ _ _ =
  error "Can only add event listener to element"

_elementSetAttribute
  :: MockJSVal -> String -> String -> IO (MockAttributes, [LogEvent])
_elementSetAttribute (MockJSElement n _ (MockAttributes atts lstns) _ logn) nm attr
  = pure
    ( MockAttributes (insert nm attr atts) lstns
    , logn <> [AttributeReceived nm attr]
    )
_elementSetAttribute _ _ _ = error "Can only add event listener to element"

_nodeAppendChild
  :: MockJSVal -> MockJSVal -> IO ([MockJSVal], [LogEvent], [LogEvent])
_nodeAppendChild (MockJSElement n _ _ kids logn) kid@(MockJSElement m _ _ _ logm)
  = pure (kids <> [kid], logn <> [ChildReceived m], logm <> [AddedAsChildTo n])
_nodeAppendChild (MockJSElement n _ _ kids logn) kid@(MockJSTextNode m _ logm)
  = pure (kids <> [kid], logn <> [ChildReceived m], logm <> [AddedAsChildTo n])
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
  (\x -> pure
    ( take x kids <> drop (x + 1) kids
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
    (\x -> pure
      ( MockAttributes atts $ delete evt lstns
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
  (\x -> pure
    ( take x kids <> [newI] <> drop x kids
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

isFree :: LogEvent -> Bool
isFree (FreeCallback _) = True
isFree _ = False

hasFree :: [LogEvent] -> Bool
hasFree l = or (fmap isFree l)

-- todo give real number
dummyClick :: MockJSVal -> IO ()
dummyClick (MockJSFunction _ f logs) = do
  if hasFree logs then error "Trying to call freed callback" else pure ()
  f $ MockMouseEvent (-1)


_htmlElemenetClick :: MockJSVal -> IO ()
_htmlElemenetClick (MockJSElement _ _ (MockAttributes _ evts) _ _) = do
  let oc = lookup "click" evts
  maybe (pure ()) dummyClick oc

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
  maybe (error $ "Cannot find object pointer in env: " <> show elt)
        return
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

----------------------

_jsValFrom
  :: (Int -> s -> [LogEvent] -> MockJSVal)
  -> IORef MockBrowserInternal
  -> s
  -> IO Int
_jsValFrom trans env toConv = do
  i <- incr env
  let elt = trans i toConv []
  wrt env i elt
  return i

_'jsValFromArray = _jsValFrom MockJSArray
_'jsValFromBool = _jsValFrom MockJSBool
_'jsValFromByteString = _jsValFrom MockJSByteString
_'jsValFromDouble = _jsValFrom MockJSDouble
_'jsValFromInt = _jsValFrom MockJSInt
_'jsValFromString = _jsValFrom MockJSString

----------------------


_'mathRandom :: IORef MockBrowserInternal -> IO Double
_'mathRandom _ = pure 0.5

_'consoleLog :: IORef MockBrowserInternal -> Int -> IO ()
_'consoleLog env v = do
  _v <- look env v
  print v


_'documentCreateTextNode :: IORef MockBrowserInternal -> String -> IO Int
_'documentCreateTextNode env txt = do
  i <- incr env
  let elt = MockJSTextNode i txt [CreatedTextNode i]
  wrt env i elt
  return i

_'getPropertyAsOpaque
  :: IORef MockBrowserInternal -> Int -> String -> IO (Maybe Int)
_'getPropertyAsOpaque env i s
  | s == "tagName" = do
    tn <- _'elementTagName env i
    _v <- _'jsValFromString env tn
    (return . Just) _v
  | s == "textContent" = do
    tc <- _'nodeTextContent env i
    _v <- _'jsValFromString env tc
    (return . Just) _v
  | s == "childNodes" = do
    cn <- _'nodeChildNodes env i
    _v <- _'jsValFromArray env cn
    (return . Just) _v
  | otherwise =  error
  $  "This property is not implemented yet in MockJSVal: "
  <> s

_'invokeOn0 :: IORef MockBrowserInternal -> Int -> String -> IO Int
_'invokeOn0 env i s
  | s == "click" = do
    _'htmlElementClick env i
    return (negate 1)
  | otherwise =  error
  $  "This function is not implemented yet in MockJSVal: "
  <> s

_'invokeOn1 :: IORef MockBrowserInternal -> Int -> String -> Int -> IO Int
_'invokeOn1 env i s v
  | s == "appendChild" = do
    _'nodeAppendChild env i v
    return (negate 1)
  | s == "removeChild" = do
    _'nodeRemoveChild env i v
    return (negate 1)
  | otherwise =  error
  $  "This function is not implemented yet in MockJSVal: "
  <> s

_'invokeOn2
  :: IORef MockBrowserInternal -> Int -> String -> Int -> Int -> IO Int
_'invokeOn2 env i s k v
  | s == "setAttribute" = do
    _k <- _'castToString env k
    _v <- _'castToString env v
    maybe
      (error "key not a string")
      (\__k -> maybe
        (error "value not a string")
        (\__v -> do
          _'elementSetAttribute env i __k __v
          return (negate 1)
        )
        _v
      )
      _k
  | s == "addEventListener" = do
    _k <- _'castToString env k
    maybe
      (error "key not a string")
      (\__k -> do
        _'eventTargetAddEventListener env i __k v
        return (negate 1)
      )
      _k
  | s == "removeEventListener" = do
    _k <- _'castToString env k
    maybe
      (error "key not a string")
      (\__k -> do
        _'eventTargetRemoveEventListener env i __k v
        return (negate 1)
      )
      _k
  | s == "insertBefore" = do
    _'nodeInsertBefore env i k v
    return (negate 1)
  | otherwise =  error
  $  "This function is not implemented yet in MockJSVal: "
  <> s

_'setValue :: IORef MockBrowserInternal -> Int -> String -> Int -> IO ()
_'setValue env o k v = do
  _o <- _'castToObject env o
  maybe
    (error "Not an object")
    (\x -> do
      __o <- look env o
      let (MockJSObject _ _ lg) = __o
      wrt env o $ MockJSObject o (insert k v x) lg
    )
    _o


_'fetch :: IORef MockBrowserInternal -> String -> RequestInit Int -> IO Int
_'fetch env _ _ = _'makeObject env


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

_'htmlElementClick :: IORef MockBrowserInternal -> Int -> IO ()
_'htmlElementClick env elt = do
  _elt <- look env elt
  _htmlElemenetClick _elt

idEq :: String -> MockJSVal -> Bool
idEq txt (MockJSElement _ _ (MockAttributes atts _) _ _) =
  Just txt == lookup "id" atts
idEq _ _ = False

_'documentBody :: IORef MockBrowserInternal -> IO Int
_'documentBody ref = do
  mb <- readIORef ref
  let browser = unBrowser mb
  maybe (error "No body.") (pure . _ptr) (lookup 0 browser)

_'documentHead :: IORef MockBrowserInternal -> IO Int
_'documentHead ref = pure (-1) -- need to implement in mock?



_documentGetElementByIdInternal :: MockJSVal -> String -> [Int]
_documentGetElementByIdInternal jsv@(MockJSElement _ _ _ ch _) txt =
  if idEq txt jsv
    then [_ptr jsv]
    else concatMap (`_documentGetElementByIdInternal` txt) ch
_documentGetElementByIdInternal _ _ = []

_'documentGetElementById
  :: IORef MockBrowserInternal -> String -> IO (Maybe Int)
_'documentGetElementById env txt = do
  body  <- _'documentBody env
  _body <- look env body
  let elts = _documentGetElementByIdInternal _body txt
  return $ if null elts then Nothing else Just $ head elts

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
  let elt = MockJSFunction i (cb . _ptr) [MadeCallback i]
  wrt env i elt
  return i

_'makeObject :: IORef MockBrowserInternal -> IO Int
_'makeObject env = do
  i <- incr env
  let elt = MockJSObject i empty []
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

_castable
  :: (MockJSVal -> IO v) -> IORef MockBrowserInternal -> Int -> IO (Maybe v)
_castable cst env elt = do
  _elt <- look env elt
  v    <- cst _elt
  pure $ Just v

_assertByteString :: MockJSVal -> IO ByteString
_assertByteString (MockJSByteString _ v _) = pure v
_assertByteString _                        = error "Not a ByteString"

_'castToByteString = _castable _assertByteString

_assertBool :: MockJSVal -> IO Bool
_assertBool (MockJSBool _ v _) = pure v
_assertBool _                  = error "Not a bool"

_'castToBool = _castable _assertBool

_assertDouble :: MockJSVal -> IO Double
_assertDouble (MockJSDouble _ v _) = pure v
_assertDouble _                    = error "Not a double"

_'castToDouble = _castable _assertDouble

_assertInt :: MockJSVal -> IO Int
_assertInt (MockJSInt _ v _) = pure v
_assertInt _                 = error "Not an int"

_'castToInt = _castable _assertInt

_assertArray :: MockJSVal -> IO [Int]
_assertArray (MockJSArray _ v _) = pure v
_assertArray _                   = error "Not an array"

_'castToArray = _castable _assertArray

_assertString :: MockJSVal -> IO String
_assertString (MockJSString _ v _) = pure v
_assertString _                    = error "Not an array"

_'castToString = _castable _assertString

_assertObject :: MockJSVal -> IO (HashMap String Int)
_assertObject (MockJSObject _ v _) = pure v
_assertObject _                    = error "Not an array"

_'castToObject = _castable _assertObject


_'defaultRequestInit = RequestInit { _ri_method      = Nothing
                                   , _ri_headers     = Nothing
                                   , _ri_body        = Nothing
                                   , _ri_mode        = Nothing
                                   , _ri_credentials = Nothing
                                   , _ri_cache       = Nothing
                                   , _ri_redirect    = Nothing
                                   , _ri_referrer    = Nothing
                                   , _ri_integrity   = Nothing
                                   }

makeMockBrowserWithContext :: IORef MockBrowserInternal -> IO (Browserful Int)
makeMockBrowserWithContext r = return Browserful
  { castToArray            = _'castToArray r
  , castToBool             = _'castToBool r
  , castToByteString       = _'castToByteString r
  , castToDouble           = _'castToDouble r
  , castToInt              = _'castToInt r
  , castToString           = _'castToString r
  , consoleLog             = _'consoleLog r
  , defaultRequestInit     = _'defaultRequestInit
  , documentCreateElement  = _'documentCreateElement r
  , documentCreateTextNode = _'documentCreateTextNode r
  , documentBody           = _'documentBody r
  , documentGetElementById = _'documentGetElementById r
  , documentHead           = _'documentHead r
  , fetch                  = _'fetch r
  , _freeCallback          = _'freeCallback r
  , getPropertyAsOpaque    = _'getPropertyAsOpaque r
  , jsValFromArray         = _'jsValFromArray r
  , jsValFromBool          = _'jsValFromBool r
  , jsValFromByteString    = _'jsValFromByteString r
  , jsValFromDouble        = _'jsValFromDouble r
  , jsValFromInt           = _'jsValFromInt r
  , jsValFromString        = _'jsValFromString r
  , invokeOn0              = _'invokeOn0 r
  , makeObject             = _'makeObject r
  , setValue               = _'setValue r
  , invokeOn1              = _'invokeOn1 r
  , invokeOn2              = _'invokeOn2 r
  , _makeHaskellCallback   = _'makeHaskellCallback r
  , mathRandom             = _'mathRandom r
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
