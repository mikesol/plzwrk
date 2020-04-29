{-# LANGUAGE OverloadedStrings #-}

module Web.Framework.Plzwrk.Domify
  ( reconcile
  , plzwrk
  )
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.DOM.Event
import qualified Data.HashMap.Strict           as HM
import           Data.IORef
import           Data.Maybe                     ( catMaybes )
import           Data.Set                hiding ( take )
import           Data.Text               hiding ( length
                                                , take
                                                )
import           Prelude                 hiding ( unwords )
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.Browserful

type MightyMouse opq
  = (MouseEvent (IO Bool) opq (IO [opq]) (IO ()) (IO ()) (IO ()))

jsValToPwMouseEvent
  :: jsval -> ReaderT (Browserful jsval) IO (MightyMouse jsval)
jsValToPwMouseEvent _ = do
  liftIO $ return MouseEvent { _mouseEvent_altKey                   = Nothing
                             , _mouseEvent_button                   = Nothing
                             , _mouseEvent_buttons                  = Nothing
                             , _mouseEvent_clientX                  = Nothing
                             , _mouseEvent_clientY                  = Nothing
                             , _mouseEvent_ctrlKey                  = Nothing
                             , _mouseEvent_metaKey                  = Nothing
                             , _mouseEvent_movementX                = Nothing
                             , _mouseEvent_movementY                = Nothing
                             , _mouseEvent_offsetX                  = Nothing
                             , _mouseEvent_offsetY                  = Nothing
                             , _mouseEvent_pageX                    = Nothing
                             , _mouseEvent_pageY                    = Nothing
                             , _mouseEvent_region                   = Nothing
                             , _mouseEvent_relatedTarget            = Nothing
                             , _mouseEvent_screenX                  = Nothing
                             , _mouseEvent_screenY                  = Nothing
                             , _mouseEvent_shiftKey                 = Nothing
                             , _mouseEvent_which                    = Nothing
                             , _mouseEvent_getModifierState         = Nothing
      -- from UIEvent
                             , _mouseEvent_detail                   = Nothing
      -- from Event
                             , _mouseEvent_bubbles                  = Nothing
                             , _mouseEvent_cancelBubble             = Nothing
                             , _mouseEvent_cancelable               = Nothing
                             , _mouseEvent_composed                 = Nothing
                             , _mouseEvent_currentTarget            = Nothing
                             , _mouseEvent_deepPath                 = Nothing
                             , _mouseEvent_defaultPrevented         = Nothing
                             , _mouseEvent_eventPhase               = Nothing
                             , _mouseEvent_explicitOriginalTarget   = Nothing
                             , _mouseEvent_originalTarget           = Nothing
                             , _mouseEvent_returnValue              = Nothing
                             , _mouseEvent_srcElement               = Nothing
                             , _mouseEvent_target                   = Nothing
                             , _mouseEvent_timeStamp                = Nothing
                             , _mouseEvent_type                     = Nothing
                             , _mouseEvent_isTrusted                = Nothing
                             , _mouseEvent_scoped                   = Nothing
                             , _mouseEvent_composedPath             = Nothing
                             , _mouseEvent_preventDefault           = Nothing
                             , _mouseEvent_stopPropagation          = Nothing
                             , _mouseEvent_stopImmediatePropagation = Nothing
                             }

data DomifiedAttributes jsval = MkDomifiedAttributes
  { _d_css     :: Maybe (HM.HashMap Text Text)
  , _d_class   :: Maybe (Set Text)
  , _d_simple  :: HM.HashMap Text Text
  , _d_onClick :: Maybe jsval
  }

data DomifiedNode jsval = DomifiedElement
    { _dom_tag  :: Text
    , _dom_attr :: (DomifiedAttributes jsval)
    , _dom_kids :: [DomifiedNode jsval]
    , _dom_ptr  :: jsval
    }
    | DomifiedTextNode Text jsval

---------- reader functions

freeAttrFunctions
  :: DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
freeAttrFunctions (MkDomifiedAttributes _ _ _ __d_onClick) = do
  _freeCallback <- asks freeCallback
  liftIO $ maybe (pure ()) _freeCallback __d_onClick

freeFunctions :: DomifiedNode jsval -> ReaderT (Browserful jsval) IO ()
freeFunctions (DomifiedElement _ b c _) = do
  freeAttrFunctions b
  mapM_ freeFunctions c
freeFunctions _ = pure ()

nodesEq
  :: Text -> Text -> DomifiedAttributes jsval -> Attributes state jsval -> Bool
nodesEq t0 t1 (MkDomifiedAttributes __d_css __d_class __d_simple _) (MkAttributes __css __class __simple _)
  = (t0 == t1)
    && (__d_css == __css)
    && (__d_class == __class)
    && (__d_simple == __simple)

reconcile
  :: IORef (Maybe (DomifiedNode jsval))
  -> (state -> Node state jsval)
  -> jsval
  -> Maybe (DomifiedNode jsval)
  -> Maybe (HydratedNode state jsval)
  -> ReaderT (Browserful jsval) IO (Maybe (DomifiedNode jsval))
reconcile refToOldDom domCreationF parentNode (Just (DomifiedElement currentTag currentAttributes currentChildren currentNode)) (Just maybeNewNode@(HydratedElement maybeNewTag maybeNewAttributes maybeNewChildren))
  = if (nodesEq currentTag maybeNewTag currentAttributes maybeNewAttributes)
    then
      (do
-- the tags and attributes are equal
        let maxlen = max (length maybeNewChildren) (length currentChildren)
        newChildren <- sequence $ getZipList
          (   (reconcile refToOldDom domCreationF currentNode)
          <$> (ZipList
                (take maxlen $ (fmap Just currentChildren) ++ repeat Nothing)
              )
          <*> (ZipList
                (take maxlen $ (fmap Just maybeNewChildren) ++ repeat Nothing)
              )
          )
        -- make new attributes to set event handlers
        currentAttributes <- hydratedAttrsToDomifiedAttrs refToOldDom
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
        res <- domify refToOldDom
                      domCreationF
                      parentNode
                      (Just currentNode)
                      maybeNewNode
        return $ Just res
      )
reconcile refToOldDom domCreationF parentNode (Just currentDomifiedText@(DomifiedTextNode currentText currentNode)) (Just maybeNewNode@(HydratedTextNode maybeNewText))
  = if (currentText == maybeNewText)
    then pure (Just currentDomifiedText)
    else
      (do
        res <- domify refToOldDom
                      domCreationF
                      parentNode
                      (Just currentNode)
                      maybeNewNode
        return $ Just res
      )
reconcile refToOldDom domCreationF parentNode (Just (DomifiedElement _ _ _ currentNode)) (Just maybeNewNode@(HydratedTextNode _))
  = do
    res <- domify refToOldDom
                  domCreationF
                  parentNode
                  (Just currentNode)
                  maybeNewNode
    return $ Just res
reconcile refToOldDom domCreationF parentNode (Just (DomifiedTextNode _ currentNode)) (Just maybeNewNode@(HydratedElement _ _ _))
  = do
    res <- domify refToOldDom
                  domCreationF
                  parentNode
                  (Just currentNode)
                  maybeNewNode
    return $ Just res
reconcile refToOldDom domCreationF parentNode Nothing (Just maybeNewNode) = do
  res <- domify refToOldDom domCreationF parentNode Nothing maybeNewNode
  return $ Just res
reconcile refToOldDom domCreationF parentNode (Just (DomifiedElement _ _ _ currentNode)) Nothing
  = do
    _removeChild <- asks removeChild
    liftIO $ _removeChild parentNode currentNode
    return Nothing
reconcile refToOldDom domCreationF parentNode (Just (DomifiedTextNode _ currentNode)) Nothing
  = do
    _removeChild <- asks removeChild
    liftIO $ _removeChild parentNode currentNode
    return Nothing
reconcile _ _ _ _ _ = error "Inconsistent state"

cbMaker
  :: IORef (Maybe (DomifiedNode jsval))
  -> (state -> Node state jsval)
  -> jsval
  -> (jsval -> ReaderT (Browserful jsval) IO z)
  -> (z -> state)
  -> Browserful jsval
  -> jsval
  -> IO ()
cbMaker refToOldDom domCreationF parentNode jsValToEvent eventToState env event
  = do
    evt <- runReaderT (jsValToEvent event) env
    let newState = eventToState evt
    oldDom <- readIORef refToOldDom
    let newHydratedDom = hydrate newState domCreationF
    newDom <- runReaderT
      (reconcile refToOldDom
                 domCreationF
                 parentNode
                 oldDom
                 (Just newHydratedDom)
      )
      env
    maybe (pure ()) (\x -> runReaderT (freeFunctions x) env) oldDom
    writeIORef refToOldDom newDom

eventable
  :: IORef (Maybe (DomifiedNode jsval))
  -> (state -> Node state jsval)
  -> jsval
  -> (jsval -> ReaderT (Browserful jsval) IO z)
  -> (z -> state)
  -> ReaderT (Browserful jsval) IO jsval
eventable refToOldDom domCreationF parentNode jsValToEvent eventToState = do
  _makeHaskellCallback <- asks makeHaskellCallback
  env                  <- ask
  liftIO $ _makeHaskellCallback
    (cbMaker refToOldDom domCreationF parentNode jsValToEvent eventToState env)

hydratedAttrsToDomifiedAttrs
  :: IORef (Maybe (DomifiedNode jsval))
  -> (state -> Node state jsval)
  -> jsval
  -> Attributes state jsval
  -> ReaderT (Browserful jsval) IO (DomifiedAttributes jsval)
hydratedAttrsToDomifiedAttrs refToOldDom domCreationF parentNode (MkAttributes __css __class __simple __onClick)
  = do
    clickFunc <- maybe
      (pure Nothing)
      (\x -> do
        func <- eventable refToOldDom
                          domCreationF
                          parentNode
                          jsValToPwMouseEvent
                          x
        return $ Just func
      )
      __onClick
    return $ MkDomifiedAttributes __css __class __simple clickFunc

setAtts :: jsval -> DomifiedAttributes jsval -> ReaderT (Browserful jsval) IO ()
setAtts currentNode domifiedAttributes@(MkDomifiedAttributes __css __class __simple __onClick)
  = do
    _setAttribute <- asks setAttribute
    liftIO $ maybe (pure ())
                   ((_setAttribute currentNode "style") . cssToStyle)
                   __css
    liftIO $ maybe (pure ())
                   ((_setAttribute currentNode "class") . unwords . toList)
                   __class
    liftIO $ mapM_ (\x -> _setAttribute currentNode (fst x) (snd x))
                   (HM.toList __simple)
    setEventHandlers currentNode domifiedAttributes

handleOnlyEventListeners
  :: (jsval -> Text -> jsval -> IO ())
  -> jsval
  -> DomifiedAttributes jsval
  -> IO ()
handleOnlyEventListeners eventListenerHandlerF currentNode domifiedAttributes =
  do
    maybe (pure ())
          (eventListenerHandlerF currentNode "onclick")
          (_d_onClick domifiedAttributes)

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
  :: IORef (Maybe (DomifiedNode jsval))
  -> (state -> Node state jsval)
  -> jsval
  -> Maybe jsval
  -> HydratedNode state jsval
  -> ReaderT (Browserful jsval) IO (DomifiedNode jsval)
domify refToOldDom domCreationF parentNode replacing (HydratedElement tag attrs children)
  = do
    _createElement <- asks createElement
    _appendChild   <- asks appendChild
    _insertBefore  <- asks insertBefore
    _removeChild   <- asks removeChild
    newNode        <- liftIO $ _createElement tag
    newAttributes  <- hydratedAttrsToDomifiedAttrs refToOldDom
                                                   domCreationF
                                                   parentNode
                                                   attrs
    setAtts newNode newAttributes
    newChildren <- mapM (domify refToOldDom domCreationF newNode Nothing)
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
  refToOldDom <- newIORef Nothing
  parentNode  <- (getElementById env) nodeId
  newDom      <- maybe
    (error $ ("Cannot find node with id " <> unpack nodeId))
    (\x -> runReaderT
      (reconcile refToOldDom domF x Nothing (Just $ hydrate state domF))
      env
    )
    parentNode
  writeIORef refToOldDom newDom
