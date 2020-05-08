module Web.Framework.Plzwrk.Domify
  ( plzwrk
  , plzwrk'
  , plzwrk'_
  , plzwrkSSR
  , plzwrkSSR'
  , plzwrkSSR'_
  )
where

import           Control.Applicative
import           Data.List.Split
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict           as HM
import           Data.IORef
import           Data.Maybe                     ( fromMaybe
                                                , catMaybes
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.Browserful
import           Web.Framework.Plzwrk.Util

data DomifiedAttribute jsval = DomifiedTextAttribute String | DomifiedFunctionAttribute jsval

data DomifiedPwNode jsval = DomifiedPwElement
    { _dom_tag  :: String
    , _dom_attr :: [(String, DomifiedAttribute jsval)]
    , _dom_kids :: [DomifiedPwNode jsval]
    , _dom_ptr  :: jsval
    }
    | DomifiedPwTextNode String jsval

data OldStuff state jsval = OldStuff {
  _oldState :: state,
  _oldDom :: Maybe (DomifiedPwNode jsval)
}

---------- reader functions


freeAttrFunction :: DomifiedAttribute jsval -> ReaderT (Browserful jsval) IO ()
freeAttrFunction (DomifiedFunctionAttribute f) = do
  __freeCallback <- asks _freeCallback
  liftIO (void $ __freeCallback f)
freeAttrFunction _ = return ()

freeFunctions :: DomifiedPwNode jsval -> ReaderT (Browserful jsval) IO ()
freeFunctions (DomifiedPwElement _ b c _) = do
  mapM_ freeAttrFunction (fmap snd b)
  mapM_ freeFunctions    c
freeFunctions _ = pure ()

data AttributeHack = MkAttributeHack
  { _hackishStyle    :: HM.HashMap String String
  , _hackishClass    :: S.Set String
  , _hackishSimple   :: HM.HashMap String String
  } deriving (Eq)

getStyleFrom :: [(String, String)] -> HM.HashMap String String
getStyleFrom l = HM.unions
  (fmap stylishAttributes (filter (\(x, _) -> x == "style") l)) where
  stylishAttributes :: (String, String) -> HM.HashMap String String
  stylishAttributes (_, y) = HM.fromList $ fmap
    (\s -> let ss = splitOn ":" s in (head ss, ss !! 1))
    (filter (elem ':') (splitOn ";" y))

getClassFrom :: [(String, String)] -> S.Set String
getClassFrom l = S.unions
  (fmap classyAttributes (filter (\(x, _) -> x == "class") l)) where
  classyAttributes :: (String, String) -> S.Set String
  classyAttributes (_, y) = S.fromList (words y)

getSimpleFrom :: [(String, String)] -> HM.HashMap String String
getSimpleFrom l = HM.unions (fmap simplyAttributes l) where
  simplyAttributes :: (String, String) -> HM.HashMap String String
  simplyAttributes (x, y) =
    if x /= "class" && x /= "style" then HM.singleton x y else HM.empty

attributeListToSplitAttrs :: [(String, String)] -> AttributeHack
attributeListToSplitAttrs fl =
  MkAttributeHack (getStyleFrom fl) (getClassFrom fl) (getSimpleFrom fl)

isDText :: (String, DomifiedAttribute jsval) -> Maybe (String, String)
isDText (k, DomifiedTextAttribute v) = Just (k, v)
isDText _                            = Nothing

isPwText :: (String, PwAttribute s jsval) -> Maybe (String, String)
isPwText (k, PwTextAttribute v) = Just (k, v)
isPwText _                      = Nothing


daToF :: [(String, DomifiedAttribute jsval)] -> [(String, String)]
daToF = mapMaybe isDText

paToF :: [(String, PwAttribute s jsval)] -> [(String, String)]
paToF = mapMaybe isPwText

nodesEq
  :: String
  -> String
  -> [(String, DomifiedAttribute jsval)]
  -> [(String, PwAttribute s jsval)]
  -> Bool
nodesEq t0 t1 a0 a1 =
  (t0 == t1)
    && (  attributeListToSplitAttrs (daToF a0)
       == attributeListToSplitAttrs (paToF a1)
       )

padr :: Int -> a -> [a] -> [a]
padr i v l = if length l >= i then l else padr i v (l ++ [v])

getOpaque :: DomifiedPwNode jsval -> jsval
getOpaque (DomifiedPwElement _ _ _ x) = x
getOpaque (DomifiedPwTextNode _ x   ) = x

reconcile
  :: Bool
  -> IORef (OldStuff state jsval)
  -> (state -> PwNode state jsval)
  -> jsval
  -> jsval
  -> Maybe (DomifiedPwNode jsval)
  -> Maybe (HydratedPwNode state jsval)
  -> ReaderT
       (Browserful jsval)
       IO
       (Maybe (DomifiedPwNode jsval))
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just (DomifiedPwElement currentTag currentAttributes currentChildren currentNode)) (Just maybeNewNode@(HydratedPwElement maybeNewTag maybeNewAttributes maybeNewChildren))
  = if nodesEq currentTag maybeNewTag currentAttributes maybeNewAttributes
    then
      (do
        let maxlen = max (length maybeNewChildren) (length currentChildren)
        newChildren <- sequence $ getZipList
          (   reconcile touchDOM
                        refToOldStuff
                        domCreationF
                        currentNode
                        topLevelNode
          <$> ZipList (padr maxlen Nothing (fmap Just currentChildren))
          <*> ZipList (padr maxlen Nothing (fmap Just maybeNewChildren))
          )
        currentAttributes <- mapM
          (hydratedAttrToDomifiedAttr refToOldStuff domCreationF parentNode)
          maybeNewAttributes
        if touchDOM
          then
            (do
              mapM_ (removeEventHandler currentNode) currentAttributes
              mapM_ (setEventHandler currentNode)    currentAttributes
            )
          else pure ()
        return $ Just
          (DomifiedPwElement currentTag
                             currentAttributes
                             (catMaybes newChildren)
                             currentNode
          )
      )
    else
      (do
        res <- domify touchDOM
                      refToOldStuff
                      domCreationF
                      parentNode
                      topLevelNode
                      (Just currentNode)
                      maybeNewNode
        return $ Just res
      )
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just currentDomifiedString@(DomifiedPwTextNode currentString currentNode)) (Just maybeNewNode@(HydratedPwTextNode maybeNewString))
  = if currentString == maybeNewString
    then pure (Just currentDomifiedString)
    else
      (do
        res <- domify touchDOM
                      refToOldStuff
                      domCreationF
                      parentNode
                      topLevelNode
                      (Just currentNode)
                      maybeNewNode
        return $ Just res
      )
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just (DomifiedPwElement _ _ _ currentNode)) (Just maybeNewNode@(HydratedPwTextNode _))
  = do
    res <- domify touchDOM
                  refToOldStuff
                  domCreationF
                  parentNode
                  topLevelNode
                  (Just currentNode)
                  maybeNewNode
    return $ Just res
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just (DomifiedPwTextNode _ currentNode)) (Just maybeNewNode@(HydratedPwElement _ _ _))
  = do
    res <- domify touchDOM
                  refToOldStuff
                  domCreationF
                  parentNode
                  topLevelNode
                  (Just currentNode)
                  maybeNewNode
    return $ Just res
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode Nothing (Just maybeNewNode)
  = do
    res <- domify touchDOM
                  refToOldStuff
                  domCreationF
                  parentNode
                  topLevelNode
                  Nothing
                  maybeNewNode
    return $ Just res
reconcile touchDOM refToOldStuff domCreationF parentNode _ (Just domifiedPwNode) Nothing
  = if touchDOM
    then
      (do
        _nodeRemoveChild <- asks nodeRemoveChild
        liftIO $ _nodeRemoveChild parentNode (getOpaque domifiedPwNode)
        return Nothing
      )
    else pure Nothing
reconcile _ _ _ _ _ _ _ = error "Inconsistent state"

reconcileAndAdd = reconcile True

cbMaker
  :: IORef (OldStuff state jsval)
  -> (state -> PwNode state jsval)
  -> jsval
  -> (jsval -> state -> IO state)
  -> Browserful jsval
  -> jsval
  -> IO ()
cbMaker refToOldStuff domCreationF topLevelNode eventToState env event = do
  oldStuff <- readIORef refToOldStuff
  let oldDom   = _oldDom oldStuff
  let oldState = _oldState oldStuff
  newState <- eventToState event oldState
  let newHydratedDom = hydrate newState domCreationF
  newDom <- runReaderT
    (reconcileAndAdd refToOldStuff
                     domCreationF
                     topLevelNode
                     topLevelNode
                     oldDom
                     (Just newHydratedDom)
    )
    env
  maybe (pure ()) (\x -> runReaderT (freeFunctions x) env) oldDom
  writeIORef refToOldStuff (OldStuff newState newDom)

eventable
  :: IORef (OldStuff state jsval)
  -> (state -> PwNode state jsval)
  -> jsval
  -> (jsval -> state -> IO state)
  -> ReaderT (Browserful jsval) IO jsval
eventable refToOldStuff domCreationF topLevelNode eventToState = do
  __makeHaskellCallback <- asks _makeHaskellCallback
  env                   <- ask
  liftIO $ __makeHaskellCallback
    (cbMaker refToOldStuff domCreationF topLevelNode eventToState env)

hydratedAttrToDomifiedAttr
  :: IORef (OldStuff state jsval)
  -> (state -> PwNode state jsval)
  -> jsval
  -> (String, PwAttribute state jsval)
  -> ReaderT (Browserful jsval) IO (String, DomifiedAttribute jsval)
hydratedAttrToDomifiedAttr refToOldStuff domCreationF topLevelNode (k, PwTextAttribute t)
  = return (k, DomifiedTextAttribute t)
hydratedAttrToDomifiedAttr refToOldStuff domCreationF topLevelNode (k, PwFunctionAttribute f)
  = do
    func <- eventable refToOldStuff domCreationF topLevelNode f
    return (k, DomifiedFunctionAttribute func)

setAtt
  :: jsval
  -> (String, DomifiedAttribute jsval)
  -> ReaderT (Browserful jsval) IO ()
setAtt currentNode (k, DomifiedTextAttribute v) = do
  _elementSetAttribute <- asks elementSetAttribute
  liftIO $ _elementSetAttribute currentNode k v
setAtt currentNode kv = setEventHandler currentNode kv

handleOnlyEventListener
  :: (jsval -> String -> jsval -> IO ())
  -> jsval
  -> (String, DomifiedAttribute jsval)
  -> IO ()
handleOnlyEventListener eventListenerHandlerF currentNode (k, DomifiedFunctionAttribute v)
  = eventListenerHandlerF currentNode k v
handleOnlyEventListener _ _ _ = pure ()

setEventHandler
  :: jsval
  -> (String, DomifiedAttribute jsval)
  -> ReaderT (Browserful jsval) IO ()
setEventHandler currentNode domifiedAttribute = do
  _eventTargetAddEventListener <- asks eventTargetAddEventListener
  liftIO $ handleOnlyEventListener _eventTargetAddEventListener
                                   currentNode
                                   domifiedAttribute

removeEventHandler
  :: jsval
  -> (String, DomifiedAttribute jsval)
  -> ReaderT (Browserful jsval) IO ()
removeEventHandler currentNode domifiedAttribute = do
  _eventTargetRemoveEventListener <- asks eventTargetRemoveEventListener
  liftIO $ handleOnlyEventListener _eventTargetRemoveEventListener
                                   currentNode
                                   domifiedAttribute

domify
  :: Bool
  -> IORef (OldStuff state jsval)
  -> (state -> PwNode state jsval)
  -> jsval
  -> jsval
  -> Maybe jsval
  -> HydratedPwNode state jsval
  -> ReaderT (Browserful jsval) IO (DomifiedPwNode jsval)
domify touchDOM refToOldStuff domCreationF parentNode topLevelNode replacing (HydratedPwElement tag attrs children)
  = do
    _documentCreateElement <- asks documentCreateElement
    _nodeAppendChild       <- asks nodeAppendChild
    _nodeInsertBefore      <- asks nodeInsertBefore
    _nodeRemoveChild       <- asks nodeRemoveChild
    newNode                <- liftIO $ _documentCreateElement tag
    newAttributes          <- mapM
      (hydratedAttrToDomifiedAttr refToOldStuff domCreationF topLevelNode)
      attrs
    if touchDOM then mapM_ (setAtt newNode) newAttributes else pure ()
    newChildren <- mapM
      (domify touchDOM refToOldStuff domCreationF newNode topLevelNode Nothing)
      children
    if touchDOM
      then maybe
        (liftIO $ _nodeAppendChild parentNode newNode)
        (\x -> do
          liftIO $ _nodeInsertBefore parentNode newNode x
          liftIO $ _nodeRemoveChild parentNode x
        )
        replacing
      else pure ()
    liftIO $ return (DomifiedPwElement tag newAttributes newChildren newNode)
domify touchDOM _ _ parentNode topLevelNode replacing (HydratedPwTextNode text)
  = do
    _documentCreateElement  <- asks documentCreateElement
    _nodeAppendChild        <- asks nodeAppendChild
    _nodeInsertBefore       <- asks nodeInsertBefore
    _nodeRemoveChild        <- asks nodeRemoveChild
    _documentCreateTextNode <- asks documentCreateTextNode
    newTextNode             <- liftIO $ _documentCreateTextNode text
    if touchDOM
      then maybe
        (liftIO $ _nodeAppendChild parentNode newTextNode)
        (\x -> do
          liftIO $ _nodeInsertBefore parentNode newTextNode x
          liftIO $ _nodeRemoveChild parentNode x
        )
        replacing
      else pure ()
    liftIO $ return (DomifiedPwTextNode text newTextNode)

getChildren :: DomifiedPwNode jsval -> [DomifiedPwNode jsval]
getChildren (DomifiedPwElement _ _ x _) = x
getChildren _                           = []

setEventHandlers_
  :: jsval -> DomifiedPwNode jsval -> ReaderT (Browserful jsval) IO ()
setEventHandlers_ v (DomifiedPwElement _ a _ _) = mapM_ (setEventHandler v) a
setEventHandlers_ _ _                           = liftIO $ pure ()

transformFromCurrentDom
  :: jsval
  -> [DomifiedPwNode jsval]
  -> ReaderT (Browserful jsval) IO [DomifiedPwNode jsval]
transformFromCurrentDom parentNode children = do
  _nodeChildNodes <- asks nodeChildNodes
  _kids           <- liftIO $ _nodeChildNodes parentNode
  let kids = fromMaybe [] _kids
  newChildren <- sequence $ getZipList
    (   transformFromCurrentDom
    <$> ZipList kids
    <*> ZipList (fmap getChildren children)
    )
  sequence_
    $ getZipList (setEventHandlers_ <$> ZipList kids <*> ZipList children)
  return $ getZipList
    (   (\cur chldrn ptr -> cur { _dom_kids = chldrn, _dom_ptr = ptr })
    <$> ZipList children
    <*> ZipList newChildren
    <*> ZipList kids
    )

addHandlers
  :: jsval
  -> DomifiedPwNode jsval
  -> ReaderT (Browserful jsval) IO (DomifiedPwNode jsval)
addHandlers parentNode curDom = do
  transformed <- transformFromCurrentDom parentNode [curDom]
  return (head transformed)

__plzwrk
  :: Bool
  -> (state -> PwNode state jsval)
  -> state
  -> jsval
  -> Browserful jsval
  -> IO (Maybe (DomifiedPwNode jsval))
__plzwrk cleanDOM domF state parentNode env = do
  refToOldStuff <- newIORef (OldStuff state Nothing)
  newDom        <- runReaderT
    (reconcile cleanDOM
               refToOldStuff
               domF
               parentNode
               parentNode
               Nothing
               (Just $ hydrate state domF)
    )
    env
  writeIORef refToOldStuff (OldStuff state newDom)
  if not cleanDOM
    then maybe
        (pure Nothing)
        (\y -> do
          withHandlers <- runReaderT (addHandlers parentNode y) env
          writeIORef refToOldStuff (OldStuff state (Just y))
          return $ Just withHandlers
        )
        newDom
    else pure newDom

_plzwrk
  :: Bool
  -> (state -> PwNode state jsval)
  -> state
  -> Browserful jsval
  -> String
  -> IO (Maybe (DomifiedPwNode jsval))
_plzwrk cleanDOM domF state env nodeId = do
  parentNode <- documentGetElementById env nodeId
  maybe (error ("Node with id not in DOM: " <> show nodeId))
        (\x -> __plzwrk cleanDOM domF state x env)
        parentNode


-- |The main function that makes a web app.

plzwrk
  :: (state -> PwNode state jsval) -- ^ A function that takes a state and produces a DOM
  -> state -- ^ An initial state
  -> Browserful jsval -- ^ A browser implementation, ie Asterius or the mock browser
  -> String -- ^ The id of the element into which the DOM is inserted. Note that plzwrk manages all children under this element. Touching the managed elements can break plzwrk.
  -> IO () -- ^ Returns nothing

plzwrk domF state env nodeId = void $ _plzwrk True domF state env nodeId

-- |A variant of plzwrk that acts on a node already rendered to the DOM,

-- ie by server-side rendering. It assumes the node has been rendered

-- with the same state-to-node function as well as the same state.

plzwrkSSR
  :: (state -> PwNode state jsval) -- ^ A function that takes a state and produces a DOM
  -> state -- ^ An initial state
  -> Browserful jsval -- ^ A browser implementation, ie Asterius or the mock browser
  -> String -- ^ The id of the element into which the DOM is inserted. Note that plzwrk manages all children under this element. Touching the managed elements can break plzwrk.
  -> IO () -- ^ Returns nothing

plzwrkSSR domF state env nodeId = void $ _plzwrk False domF state env nodeId

_plzwrk'
  :: Bool
  -> (state -> PwNode state jsval)
  -> state
  -> Browserful jsval
  -> IO (Maybe (DomifiedPwNode jsval))
_plzwrk' cleanDOM domF state env = do
  parentNode <- documentBody env
  __plzwrk cleanDOM domF state parentNode env

-- |A variation of plzwrk that inserts the node as a child of the document's body.

plzwrk' :: (state -> PwNode state jsval) -> state -> Browserful jsval -> IO ()
plzwrk' domF state env = void $ _plzwrk' True domF state env

-- |A variation of plzwrk that inserts the node as a child of the document's body.

plzwrkSSR'
  :: (state -> PwNode state jsval) -> state -> Browserful jsval -> IO ()
plzwrkSSR' domF state env = void $ _plzwrk' False domF state env

-- |A variation of plzwrk that takes no state.

plzwrk'_ :: (() -> PwNode () jsval) -> Browserful jsval -> IO ()
plzwrk'_ domF = plzwrk' domF ()

-- |A variation of plzwrkSSR that takes no state.

plzwrkSSR'_ :: (() -> PwNode () jsval) -> Browserful jsval -> IO ()
plzwrkSSR'_ domF = plzwrkSSR' domF ()
