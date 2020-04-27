{-# LANGUAGE OverloadedStrings #-}

module DomWAI
    (
        plzwrk
    ) where

import           Asterius.Types
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.IORef
import           Data.Set                  hiding (take)
import           Data.Text                 hiding (length, take)
import           DomBase
import           Prelude                   hiding (unwords)
import           WebAPI

jsValToClickEvent :: JSVal -> ClickEvent
jsValToClickEvent _ = ClickEvent -- todo

data DomifiedAttributes state = MkDomifiedAttributes
    { _d_css     :: Maybe CSS
    , _d_class   :: Maybe (Set Text)
    , _d_simple  :: Set (Text, Text)
    , _d_onClick :: Maybe JSFunction
    }

data DomifiedNode state = DomifiedElement Text (DomifiedAttributes state)
                  [DomifiedNode state] JSVal
    | DomifiedTextNode Text JSVal

freeAttrFunctions :: DomifiedAttributes state -> IO ()
freeAttrFunctions (MkDomifiedAttributes _ _ _ __d_onClick) = do
    maybe (pure ()) freeHaskellCallback __d_onClick

freeFunctions :: DomifiedNode state -> IO ()
freeFunctions (DomifiedElement _ b c _) = do
    freeAttrFunctions b
    mapM_ freeFunctions c
freeFunctions _ = pure ()

nodesEq :: Text -> Text -> DomifiedAttributes state -> Attributes state -> Bool
nodesEq t0 t1 (MkDomifiedAttributes __d_css __d_class __d_simple _) (MkAttributes __css __class __simple _) = (t0 == t1) && (__d_css == __css) && (__d_class == __class) && (__d_simple == __simple)

reconcile :: IORef (Maybe (DomifiedNode state)) -> (state -> Node state) -> JSVal -> Maybe (DomifiedNode state) -> HydratedNode state -> IO (DomifiedNode state)
reconcile refToOldDom domCreationF parentNode (Just (DomifiedElement currentTag currentAttributes currentChildren currentNode)) maybeNewNode@(HydratedElement maybeNewTag maybeNewAttributes maybeNewChildren) = if (nodesEq currentTag maybeNewTag currentAttributes maybeNewAttributes) then (do
            -- the tags and attributes are equal
            newChildren <- sequence $ getZipList ((reconcile refToOldDom domCreationF currentNode) <$> (ZipList (take (length maybeNewChildren) $ (fmap Just currentChildren) ++ repeat Nothing)) <*> (ZipList maybeNewChildren))
            -- make new attributes to set event handlers
            currentAttributes <- hydratedAttrsToDomifiedAttrs refToOldDom domCreationF parentNode maybeNewAttributes
            removeEventHandlers currentNode currentAttributes
            setEventHandlers currentNode currentAttributes
            return $ DomifiedElement currentTag currentAttributes newChildren currentNode)
        else (domify refToOldDom domCreationF parentNode (Just currentNode) maybeNewNode)
reconcile refToOldDom domCreationF parentNode (Just currentDomifiedText@(DomifiedTextNode currentText currentNode)) maybeNewNode@(HydratedTextNode maybeNewText) =
    if (currentText == maybeNewText) then pure currentDomifiedText else (domify refToOldDom domCreationF parentNode (Just currentNode) maybeNewNode)
reconcile refToOldDom domCreationF parentNode (Just (DomifiedElement _ _ _ currentNode)) maybeNewNode@(HydratedTextNode _) =
    domify refToOldDom domCreationF parentNode (Just currentNode) maybeNewNode
reconcile refToOldDom domCreationF parentNode (Just (DomifiedTextNode _ currentNode)) maybeNewNode@(HydratedElement _ _ _) =
    domify refToOldDom domCreationF parentNode (Just currentNode) maybeNewNode
reconcile refToOldDom domCreationF parentNode Nothing maybeNewNode = domify refToOldDom domCreationF parentNode Nothing maybeNewNode

eventable :: IORef (Maybe (DomifiedNode state)) -> (state -> Node state) -> JSVal -> (JSVal -> z) -> (z -> state) -> IO JSFunction
eventable refToOldDom domCreationF parentNode jsValToEvent eventToState = makeHaskellCallback1 (\event -> do
    let newState = eventToState (jsValToEvent event)
    oldDom <- readIORef refToOldDom
    let newHydratedDom = hydrate newState domCreationF
    newDom <- reconcile refToOldDom domCreationF parentNode oldDom newHydratedDom
    maybe (pure ()) freeFunctions oldDom
    writeIORef refToOldDom (Just newDom))

hydratedAttrsToDomifiedAttrs :: IORef (Maybe (DomifiedNode state)) -> (state -> Node state) -> JSVal -> Attributes state -> IO (DomifiedAttributes state)
hydratedAttrsToDomifiedAttrs refToOldDom domCreationF parentNode (MkAttributes __css __class __simple __onClick) = do
    clickFunc <- maybe (pure Nothing) (\x -> do
        func <- eventable refToOldDom domCreationF parentNode jsValToClickEvent x
        return $ Just func) __onClick
    return $ MkDomifiedAttributes __css __class __simple clickFunc

setAtts :: JSVal -> DomifiedAttributes state -> IO ()
setAtts currentNode domifiedAttributes@(MkDomifiedAttributes __css __class __simple __onClick) = do
    maybe (pure ()) ((setAttribute currentNode "style") . cssToStyle) __css
    maybe (pure ()) ((setAttribute currentNode "class") . unwords . toList) __class
    mapM_ (\x -> setAttribute currentNode (fst x) (snd x)) (toList __simple)
    setEventHandlers currentNode domifiedAttributes

handleOnlyEventListeners :: (JSVal -> Text -> JSFunction -> IO ()) -> JSVal -> DomifiedAttributes state -> IO ()
handleOnlyEventListeners eventListenerHandlerF currentNode domifiedAttributes = do
    maybe (pure ()) (eventListenerHandlerF currentNode "onclick") (_d_onClick domifiedAttributes)

setEventHandlers :: JSVal -> DomifiedAttributes state -> IO ()
setEventHandlers = handleOnlyEventListeners addEventListener

removeEventHandlers :: JSVal -> DomifiedAttributes state -> IO ()
removeEventHandlers = handleOnlyEventListeners removeEventListener

domify :: IORef (Maybe (DomifiedNode state)) -> (state -> Node state) -> JSVal -> Maybe JSVal -> HydratedNode state -> IO (DomifiedNode state)
domify refToOldDom domCreationF parentNode replacing (HydratedElement tag attrs children) = do
    newNode <- createElement tag
    newAttributes <- hydratedAttrsToDomifiedAttrs refToOldDom domCreationF parentNode attrs
    setAtts newNode newAttributes
    newChildren <- mapM (domify refToOldDom domCreationF newNode Nothing) children
    maybe (appendChild parentNode newNode) (\x -> do
        insertBefore parentNode newNode x
        removeChild parentNode x) replacing
    return $ DomifiedElement tag newAttributes newChildren newNode
domify _ _ parentNode replacing (HydratedTextNode text) = do
    newTextNode <- createTextNode text
    maybe (appendChild parentNode newTextNode) (\x -> do
        insertBefore parentNode newTextNode x
        removeChild parentNode x) replacing
    return $ DomifiedTextNode text newTextNode

plzwrk :: state -> (state -> Node state) -> Text -> IO ()
plzwrk state domCreationF parname = do
    refToOldDom <- newIORef Nothing
    parentNode <- getElementById parname
    void $ domify refToOldDom domCreationF parentNode Nothing (hydrate state domCreationF)
