{-# LANGUAGE OverloadedStrings #-}

module Web.Framework.Plzwrk.Domify
  ( reconcile
  , plzwrk
  , plzwrk'
  , plzwrk'_
  , OldStuff(..)
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
import           Data.Set                hiding ( take )
import qualified Data.Set                      as S
import           Data.Text               hiding ( length
                                                , take
                                                )
import           Prelude                 hiding ( unwords )
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.Browserful

data DomifiedAttributes jsval = MkDomifiedAttributes
  { _d_style     :: HM.HashMap Text Text
  , _d_class     :: Set Text
  , _d_simple    :: HM.HashMap Text Text
  , _d_handlers  :: HM.HashMap Text jsval
  }

data DomifiedNode jsval = DomifiedElement
    { _dom_tag  :: Text
    , _dom_attr :: (DomifiedAttributes jsval)
    , _dom_kids :: [DomifiedNode jsval]
    , _dom_ptr  :: jsval
    }
    | DomifiedTextNode Text jsval

data OldStuff state jsval = OldStuff {
  _oldState :: state,
  _oldDom :: Maybe (DomifiedNode jsval)
}

---------- reader functions


freeAttrFunctions
  :: DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
freeAttrFunctions (MkDomifiedAttributes _ _ _ __d_handlers) = do
  _freeCallback <- asks freeCallback
  liftIO $ void (mapM _freeCallback (HM.elems __d_handlers))

freeFunctions :: DomifiedNode jsval -> ReaderT (Browserful jsval) IO ()
freeFunctions (DomifiedElement _ b c _) = do
  freeAttrFunctions b
  mapM_ freeFunctions c
freeFunctions _ = pure ()

nodesEq
  :: Text -> Text -> DomifiedAttributes jsval -> Attributes state jsval -> Bool
nodesEq t0 t1 (MkDomifiedAttributes __d_style __d_class __d_simple _) (MkAttributes __style __class __simple _)
  = (t0 == t1)
    && (__d_style == __style)
    && (__d_class == __class)
    && (__d_simple == __simple)

reconcile
  :: IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
  -> jsval
  -> Maybe (DomifiedNode jsval)
  -> Maybe (HydratedNode state jsval)
  -> ReaderT (Browserful jsval) IO (Maybe (DomifiedNode jsval))
reconcile refToOldStuff domCreationF parentNode (Just (DomifiedElement currentTag currentAttributes currentChildren currentNode)) (Just maybeNewNode@(HydratedElement maybeNewTag maybeNewAttributes maybeNewChildren))
  = if (nodesEq currentTag maybeNewTag currentAttributes maybeNewAttributes)
    then
      (do
-- the tags and attributes are equal

        let maxlen = max (length maybeNewChildren) (length currentChildren)
        newChildren <- sequence $ getZipList
          (   (reconcile refToOldStuff domCreationF currentNode)
          <$> (ZipList
                (take maxlen $ (fmap Just currentChildren) ++ repeat Nothing)
              )
          <*> (ZipList
                (take maxlen $ (fmap Just maybeNewChildren) ++ repeat Nothing)
              )
          )
        -- make new attributes to set event handlers

        currentAttributes <- hydratedAttrsToDomifiedAttrs refToOldStuff
                                                          domCreationF
                                                          parentNode
                                                          maybeNewAttributes
        removeEventHandlers currentNode currentAttributes
        setEventHandlers currentNode currentAttributes
        return $ Just
          (DomifiedElement currentTag
                           currentAttributes
                           (catMaybes newChildren)
                           currentNode
          )
      )
    else
      (do
        res <- domify refToOldStuff
                      domCreationF
                      parentNode
                      (Just currentNode)
                      maybeNewNode
        return $ Just res
      )
reconcile refToOldStuff domCreationF parentNode (Just currentDomifiedText@(DomifiedTextNode currentText currentNode)) (Just maybeNewNode@(HydratedTextNode maybeNewText))
  = if (currentText == maybeNewText)
    then pure (Just currentDomifiedText)
    else
      (do
        res <- domify refToOldStuff
                      domCreationF
                      parentNode
                      (Just currentNode)
                      maybeNewNode
        return $ Just res
      )
reconcile refToOldStuff domCreationF parentNode (Just (DomifiedElement _ _ _ currentNode)) (Just maybeNewNode@(HydratedTextNode _))
  = do
    res <- domify refToOldStuff
                  domCreationF
                  parentNode
                  (Just currentNode)
                  maybeNewNode
    return $ Just res
reconcile refToOldStuff domCreationF parentNode (Just (DomifiedTextNode _ currentNode)) (Just maybeNewNode@(HydratedElement _ _ _))
  = do
    res <- domify refToOldStuff
                  domCreationF
                  parentNode
                  (Just currentNode)
                  maybeNewNode
    return $ Just res
reconcile refToOldStuff domCreationF parentNode Nothing (Just maybeNewNode) =
  do
    res <- domify refToOldStuff domCreationF parentNode Nothing maybeNewNode
    return $ Just res
reconcile refToOldStuff domCreationF parentNode (Just (DomifiedElement _ _ _ currentNode)) Nothing
  = do
    _removeChild <- asks removeChild
    liftIO $ _removeChild parentNode currentNode
    return Nothing
reconcile refToOldStuff domCreationF parentNode (Just (DomifiedTextNode _ currentNode)) Nothing
  = do
    _removeChild <- asks removeChild
    liftIO $ _removeChild parentNode currentNode
    return Nothing
reconcile _ _ _ _ _ = error "Inconsistent state"

cbMaker
  :: IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
  -> jsval
  -> (jsval -> state -> IO state)
  -> Browserful jsval
  -> jsval
  -> IO ()
cbMaker refToOldStuff domCreationF parentNode eventToState env event = do
  oldStuff <- readIORef refToOldStuff
  let oldDom   = _oldDom oldStuff
  let oldState = _oldState oldStuff
  newState <- eventToState event oldState
  let newHydratedDom = hydrate newState domCreationF
  newDom <- runReaderT
    (reconcile refToOldStuff
               domCreationF
               parentNode
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
eventable refToOldStuff domCreationF parentNode eventToState = do
  _makeHaskellCallback <- asks makeHaskellCallback
  env                  <- ask
  liftIO $ _makeHaskellCallback
    (cbMaker refToOldStuff domCreationF parentNode eventToState env)

hydratedAttrsToDomifiedAttrs
  :: IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
  -> jsval
  -> Attributes state jsval
  -> ReaderT (Browserful jsval) IO (DomifiedAttributes jsval)
hydratedAttrsToDomifiedAttrs refToOldStuff domCreationF parentNode (MkAttributes __style __class __simple __handlers)
  = do
    handlers <- mapM
      (\(k, v) -> do
        func <- eventable refToOldStuff domCreationF parentNode v
        return $ (k, func)
      )
      (HM.toList __handlers)
    return
      $ MkDomifiedAttributes __style __class __simple (HM.fromList handlers)

setAtts :: jsval -> DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
setAtts currentNode domifiedAttributes@(MkDomifiedAttributes __style __class __simple _)
  = do
    _setAttribute <- asks setAttribute
    liftIO $ if (HM.null __style)
      then (pure ())
      else (_setAttribute currentNode "style") . cssToStyle $ __style
    liftIO $ if (S.null __class)
      then (pure ())
      else ((_setAttribute currentNode "class") . unwords . toList) $ __class
    liftIO $ mapM_ (\x -> _setAttribute currentNode (fst x) (snd x))
                   (HM.toList __simple)
    setEventHandlers currentNode domifiedAttributes

handleOnlyEventListeners
  :: (jsval -> Text -> jsval -> IO ())
  -> jsval
  -> DomifiedAttributes jsval
  -> IO ()
handleOnlyEventListeners eventListenerHandlerF currentNode domifiedAttributes =
  void $ mapM (\(k, v) -> eventListenerHandlerF currentNode k v)
              (HM.toList . _d_handlers $ domifiedAttributes)

setEventHandlers
  :: jsval -> DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
setEventHandlers currentNode domifiedAttributes = do
  _addEventListener <- asks addEventListener
  liftIO $ handleOnlyEventListeners _addEventListener
                                    currentNode
                                    domifiedAttributes

removeEventHandlers
  :: jsval -> DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
removeEventHandlers currentNode domifiedAttributes = do
  _removeEventListener <- asks removeEventListener
  liftIO $ handleOnlyEventListeners _removeEventListener
                                    currentNode
                                    domifiedAttributes

domify
  :: IORef (OldStuff state jsval)
  -> (state -> Node state jsval)
  -> jsval
  -> Maybe jsval
  -> HydratedNode state jsval
  -> ReaderT (Browserful jsval) IO (DomifiedNode jsval)
domify refToOldStuff domCreationF parentNode replacing (HydratedElement tag attrs children)
  = do
    _createElement <- asks createElement
    _appendChild   <- asks appendChild
    _insertBefore  <- asks insertBefore
    _removeChild   <- asks removeChild
    newNode        <- liftIO $ _createElement tag
    newAttributes  <- hydratedAttrsToDomifiedAttrs refToOldStuff
                                                   domCreationF
                                                   parentNode
                                                   attrs
    setAtts newNode newAttributes
    newChildren <- mapM (domify refToOldStuff domCreationF newNode Nothing)
                        children
    maybe
      (liftIO $ _appendChild parentNode newNode)
      (\x -> do
        liftIO $ _insertBefore parentNode newNode x
        liftIO $ _removeChild parentNode x
      )
      replacing
    liftIO $ return (DomifiedElement tag newAttributes newChildren newNode)
domify _ _ parentNode replacing (HydratedTextNode text) = do
  _createElement  <- asks createElement
  _appendChild    <- asks appendChild
  _insertBefore   <- asks insertBefore
  _removeChild    <- asks removeChild
  _createTextNode <- asks createTextNode
  newTextNode     <- liftIO $ _createTextNode text
  maybe
    (liftIO $ _appendChild parentNode newTextNode)
    (\x -> do
      liftIO $ _insertBefore parentNode newTextNode x
      liftIO $ _removeChild parentNode x
    )
    replacing
  liftIO $ return (DomifiedTextNode text newTextNode)

plzwrk
  :: (state -> Node state jsval) -> state -> Browserful jsval -> Text -> IO ()
plzwrk domF state env nodeId = do
  refToOldStuff <- newIORef (OldStuff state Nothing)
  parentNode    <- (getElementById env) nodeId
  newDom        <- maybe
    (error $ ("Cannot find node with id " <> unpack nodeId))
    (\x -> runReaderT
      (reconcile refToOldStuff domF x Nothing (Just $ hydrate state domF))
      env
    )
    parentNode
  writeIORef refToOldStuff (OldStuff state newDom)

plzwrk' :: (state -> Node state jsval) -> state -> Browserful jsval -> IO ()
plzwrk' domF state env = do
  refToOldStuff <- newIORef (OldStuff state Nothing)
  parentNode    <- (getBody env)
  newDom        <- runReaderT
    (reconcile refToOldStuff domF parentNode Nothing (Just $ hydrate state domF)
    )
    env
  writeIORef refToOldStuff (OldStuff state newDom)

plzwrk'_ :: (Int -> Node Int jsval) -> Browserful jsval -> IO ()
plzwrk'_ domF env = plzwrk' domF 0 env
