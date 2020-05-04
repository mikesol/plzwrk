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
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict           as HM
import           Data.IORef
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.Browserful
import           Web.Framework.Plzwrk.Util

data DomifiedAttributes jsval = MkDomifiedAttributes
  { _d_style     :: HM.HashMap String String
  , _d_class     :: S.Set String
  , _d_simple    :: HM.HashMap String String
  , _d_handlers  :: HM.HashMap String jsval
  }

data DomifiedNode jsval = DomifiedElement
    { _dom_tag  :: String
    , _dom_attr :: (DomifiedAttributes jsval)
    , _dom_kids :: [DomifiedNode jsval]
    , _dom_ptr  :: jsval
    }
    | DomifiedTextNode String jsval

data OldStuff state jsval = OldStuff {
  _oldState :: state,
  _oldDom :: Maybe (DomifiedNode jsval)
}

---------- reader functions




freeAttrFunctions
  :: DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
freeAttrFunctions (MkDomifiedAttributes _ _ _ __d_handlers) = do
  __freeCallback <- asks _freeCallback
  liftIO $ void (mapM __freeCallback (HM.elems __d_handlers))

freeFunctions :: DomifiedNode jsval -> ReaderT (Browserful jsval) IO ()
freeFunctions (DomifiedElement _ b c _) = do
  freeAttrFunctions b
  mapM_ freeFunctions c
freeFunctions _ = pure ()

nodesEq
  :: String
  -> String
  -> DomifiedAttributes jsval
  -> Attributes state jsval
  -> Bool
nodesEq t0 t1 (MkDomifiedAttributes __d_style __d_class __d_simple _) (MkAttributes __style __class __simple _)
  = (t0 == t1)
    && (__d_style == __style)
    && (__d_class == __class)
    && (__d_simple == __simple)

padr :: Int -> a -> [a] -> [a]
padr i v l = if (length l >= i) then l else padr i v (l ++ [v])

reconcile
  :: Bool
  -> IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
  -> jsval
  -> jsval
  -> Maybe (DomifiedNode jsval)
  -> Maybe (HydratedNode state jsval)
  -> ReaderT
       (Browserful jsval)
       IO
       (Maybe (DomifiedNode jsval))
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just (DomifiedElement currentTag currentAttributes currentChildren currentNode)) (Just maybeNewNode@(HydratedElement maybeNewTag maybeNewAttributes maybeNewChildren))
  = if (nodesEq currentTag maybeNewTag currentAttributes maybeNewAttributes)
    then
      (do
        let maxlen = max (length maybeNewChildren) (length currentChildren)
        newChildren <- sequence $ getZipList
          (   (reconcile touchDOM
                         refToOldStuff
                         domCreationF
                         currentNode
                         topLevelNode
              )
          <$> (ZipList (padr maxlen Nothing (fmap Just currentChildren)))
          <*> (ZipList (padr maxlen Nothing (fmap Just maybeNewChildren)))
          )
        currentAttributes <- hydratedAttrsToDomifiedAttrs refToOldStuff
                                                          domCreationF
                                                          parentNode
                                                          maybeNewAttributes
        if touchDOM
          then
            (do
              removeEventHandlers currentNode currentAttributes
              setEventHandlers currentNode currentAttributes
            )
          else (pure ())
        return $ Just
          (DomifiedElement currentTag
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
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just currentDomifiedString@(DomifiedTextNode currentString currentNode)) (Just maybeNewNode@(HydratedTextNode maybeNewString))
  = if (currentString == maybeNewString)
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
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just (DomifiedElement _ _ _ currentNode)) (Just maybeNewNode@(HydratedTextNode _))
  = do
    res <- domify touchDOM
                  refToOldStuff
                  domCreationF
                  parentNode
                  topLevelNode
                  (Just currentNode)
                  maybeNewNode
    return $ Just res
reconcile touchDOM refToOldStuff domCreationF parentNode topLevelNode (Just (DomifiedTextNode _ currentNode)) (Just maybeNewNode@(HydratedElement _ _ _))
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
reconcile touchDOM refToOldStuff domCreationF parentNode _ (Just (DomifiedElement _ _ _ currentNode)) Nothing
  = if (touchDOM)
    then
      (do
        _nodeRemoveChild <- asks nodeRemoveChild
        liftIO $ _nodeRemoveChild parentNode currentNode
        return Nothing
      )
    else (pure Nothing)
reconcile touchDOM refToOldStuff domCreationF parentNode _ (Just (DomifiedTextNode _ currentNode)) Nothing
  = if (touchDOM)
    then
      (do
        _nodeRemoveChild <- asks nodeRemoveChild
        liftIO $ _nodeRemoveChild parentNode currentNode
        return Nothing
      )
    else (pure Nothing)
reconcile _ _ _ _ _ _ _ = error "Inconsistent state"

reconcileAndAdd = reconcile True

cbMaker
  :: IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
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
  -> (state -> Node state jsval)
  -> jsval
  -> (jsval -> state -> IO state)
  -> ReaderT (Browserful jsval) IO jsval
eventable refToOldStuff domCreationF topLevelNode eventToState = do
  __makeHaskellCallback <- asks _makeHaskellCallback
  env                   <- ask
  liftIO $ __makeHaskellCallback
    (cbMaker refToOldStuff domCreationF topLevelNode eventToState env)

hydratedAttrsToDomifiedAttrs
  :: IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
  -> jsval
  -> Attributes state jsval
  -> ReaderT (Browserful jsval) IO (DomifiedAttributes jsval)
hydratedAttrsToDomifiedAttrs refToOldStuff domCreationF topLevelNode (MkAttributes __style __class __simple __handlers)
  = do
    handlers <- mapM
      (\(k, v) -> do
        func <- eventable refToOldStuff domCreationF topLevelNode v
        return $ (k, func)
      )
      (HM.toList __handlers)
    return
      $ MkDomifiedAttributes __style __class __simple (HM.fromList handlers)

setAtts :: jsval -> DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
setAtts currentNode domifiedAttributes@(MkDomifiedAttributes __style __class __simple _)
  = do
    _elementSetAttribute <- asks elementSetAttribute
    liftIO $ if (HM.null __style)
      then (pure ())
      else (_elementSetAttribute currentNode "style") . cssToStyle $ __style
    liftIO $ if (S.null __class)
      then (pure ())
      else
        ((_elementSetAttribute currentNode "class") . unwords . S.toList)
          $ __class
    liftIO $ mapM_ (\x -> _elementSetAttribute currentNode (fst x) (snd x))
                   (HM.toList __simple)
    setEventHandlers currentNode domifiedAttributes

handleOnlyEventListeners
  :: (jsval -> String -> jsval -> IO ())
  -> jsval
  -> DomifiedAttributes jsval
  -> IO ()
handleOnlyEventListeners eventListenerHandlerF currentNode domifiedAttributes =
  void $ mapM (\(k, v) -> eventListenerHandlerF currentNode k v)
              (HM.toList . _d_handlers $ domifiedAttributes)

setEventHandlers
  :: jsval -> DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
setEventHandlers currentNode domifiedAttributes = do
  _eventTargetAddEventListener <- asks eventTargetAddEventListener
  liftIO $ handleOnlyEventListeners _eventTargetAddEventListener
                                    currentNode
                                    domifiedAttributes

removeEventHandlers
  :: jsval -> DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
removeEventHandlers currentNode domifiedAttributes = do
  _eventTargetRemoveEventListener <- asks eventTargetRemoveEventListener
  liftIO $ handleOnlyEventListeners _eventTargetRemoveEventListener
                                    currentNode
                                    domifiedAttributes

domify
  :: Bool
  -> IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
  -> jsval
  -> jsval
  -> Maybe jsval
  -> HydratedNode state jsval
  -> ReaderT (Browserful jsval) IO (DomifiedNode jsval)
domify touchDOM refToOldStuff domCreationF parentNode topLevelNode replacing (HydratedElement tag attrs children)
  = do
    _documentCreateElement <- asks documentCreateElement
    _nodeAppendChild       <- asks nodeAppendChild
    _nodeInsertBefore      <- asks nodeInsertBefore
    _nodeRemoveChild       <- asks nodeRemoveChild
    newNode                <- liftIO $ _documentCreateElement tag
    newAttributes          <- hydratedAttrsToDomifiedAttrs refToOldStuff
                                                           domCreationF
                                                           topLevelNode
                                                           attrs
    if touchDOM then (setAtts newNode newAttributes) else (pure ())
    newChildren <- mapM
      (domify touchDOM refToOldStuff domCreationF newNode topLevelNode Nothing)
      children
    if touchDOM
      then
        (do
          maybe
            (liftIO $ _nodeAppendChild parentNode newNode)
            (\x -> do
              liftIO $ _nodeInsertBefore parentNode newNode x
              liftIO $ _nodeRemoveChild parentNode x
            )
            replacing
        )
      else (pure ())
    liftIO $ return (DomifiedElement tag newAttributes newChildren newNode)
domify touchDOM _ _ parentNode topLevelNode replacing (HydratedTextNode text) =
  do
    _documentCreateElement  <- asks documentCreateElement
    _nodeAppendChild        <- asks nodeAppendChild
    _nodeInsertBefore       <- asks nodeInsertBefore
    _nodeRemoveChild        <- asks nodeRemoveChild
    _documentCreateTextNode <- asks documentCreateTextNode
    newTextNode             <- liftIO $ _documentCreateTextNode text
    if touchDOM
      then
        (do
          maybe
            (liftIO $ _nodeAppendChild parentNode newTextNode)
            (\x -> do
              liftIO $ _nodeInsertBefore parentNode newTextNode x
              liftIO $ _nodeRemoveChild parentNode x
            )
            replacing
        )
      else (pure ())
    liftIO $ return (DomifiedTextNode text newTextNode)

getChildren :: DomifiedNode jsval -> [DomifiedNode jsval]
getChildren (DomifiedElement _ _ x _) = x
getChildren _                         = []

setEventHandlers_
  :: jsval -> DomifiedNode jsval -> ReaderT (Browserful jsval) IO ()
setEventHandlers_ v (DomifiedElement _ a _ _) = setEventHandlers v a
setEventHandlers_ _ _                         = liftIO $ pure ()

transformFromCurrentDom
  :: jsval
  -> [DomifiedNode jsval]
  -> ReaderT (Browserful jsval) IO [DomifiedNode jsval]
transformFromCurrentDom parentNode children = do
  _nodeChildNodes <- asks nodeChildNodes
  _kids            <- liftIO $ _nodeChildNodes parentNode
  let kids = maybe [] id _kids
  newChildren     <- sequence $ getZipList
    (   transformFromCurrentDom
    <$> (ZipList kids)
    <*> (ZipList $ fmap getChildren children)
    )
  sequence
    $ getZipList (setEventHandlers_ <$> (ZipList kids) <*> (ZipList children))
  return $ getZipList
    (   (\cur chldrn ptr -> cur { _dom_kids = chldrn, _dom_ptr = ptr })
    <$> (ZipList children)
    <*> (ZipList newChildren)
    <*> (ZipList kids)
    )

addHandlers
  :: jsval
  -> DomifiedNode jsval
  -> ReaderT (Browserful jsval) IO (DomifiedNode jsval)
addHandlers parentNode curDom = do
  transformed <- transformFromCurrentDom parentNode [curDom]
  return $ (transformed !! 0)

__plzwrk
  :: Bool
  -> (state -> Node state jsval)
  -> state
  -> jsval
  -> Browserful jsval
  -> IO (Maybe (DomifiedNode jsval))
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
  if (not cleanDOM)
    then
      (maybe
        (pure Nothing)
        (\y -> do
          withHandlers <- runReaderT (addHandlers parentNode y) env
          writeIORef refToOldStuff (OldStuff state (Just y))
          return $ Just withHandlers
        )
        newDom
      )
    else pure newDom

_plzwrk
  :: Bool
  -> (state -> Node state jsval)
  -> state
  -> Browserful jsval
  -> String
  -> IO (Maybe (DomifiedNode jsval))
_plzwrk cleanDOM domF state env nodeId = do
  parentNode <- (documentGetElementById env) nodeId
  maybe (error ("Node with id not in DOM: " <> show nodeId))
        (\x -> __plzwrk cleanDOM domF state x env)
        parentNode


-- |The main function that makes a web app.
plzwrk
  :: (state -> Node state jsval) -- ^ A function that takes a state and produces a DOM
  -> state -- ^ An initial state
  -> Browserful jsval -- ^ A browser implementation, ie Asterius or the mock browser
  -> String -- ^ The id of the element into which the DOM is inserted. Note that plzwrk manages all children under this element. Touching the managed elements can break plzwrk.
  -> IO () -- ^ Returns nothing
plzwrk domF state env nodeId = void $ _plzwrk True domF state env nodeId

-- |A variant of plzwrk that acts on a node already rendered to the DOM,
-- ie by server-side rendering. It assumes the node has been rendered
-- with the same state-to-node function as well as the same state.
plzwrkSSR
  :: (state -> Node state jsval) -- ^ A function that takes a state and produces a DOM
  -> state -- ^ An initial state
  -> Browserful jsval -- ^ A browser implementation, ie Asterius or the mock browser
  -> String -- ^ The id of the element into which the DOM is inserted. Note that plzwrk manages all children under this element. Touching the managed elements can break plzwrk.
  -> IO () -- ^ Returns nothing
plzwrkSSR domF state env nodeId = void $ _plzwrk False domF state env nodeId

_plzwrk'
  :: Bool
  -> (state -> Node state jsval)
  -> state
  -> Browserful jsval
  -> IO (Maybe (DomifiedNode jsval))
_plzwrk' cleanDOM domF state env = do
  parentNode <- (documentBody env)
  __plzwrk cleanDOM domF state parentNode env

-- |A variation of plzwrk that inserts the node as a child of the document's body.
plzwrk' :: (state -> Node state jsval) -> state -> Browserful jsval -> IO ()
plzwrk' domF state env = void $ _plzwrk' True domF state env

-- |A variation of plzwrk that inserts the node as a child of the document's body.
plzwrkSSR' :: (state -> Node state jsval) -> state -> Browserful jsval -> IO ()
plzwrkSSR' domF state env = void $ _plzwrk' False domF state env

-- |A variation of plzwrk that takes no state.
plzwrk'_ :: (() -> Node () jsval) -> Browserful jsval -> IO ()
plzwrk'_ domF env = plzwrk' domF () env

-- |A variation of plzwrkSSR that takes no state.
plzwrkSSR'_ :: (() -> Node () jsval) -> Browserful jsval -> IO ()
plzwrkSSR'_ domF env = plzwrkSSR' domF () env
