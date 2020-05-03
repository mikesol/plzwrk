module Web.Framework.Plzwrk.Util
  ( (<.>)
  , wStyle
  , wStyle'
  , wStyles
  , wStyles'
  , wClass
  , wClass'
  , wClasses
  , wClasses'
  , wOnClick
  , wOnClick'
  , wId
  , wId'
  , wOnInput
  , wOnInput'
  , wAttr
  , wAttr'
  , wAttrs
  , wAttrs'
  , eventTargetValue
  , eventPreventDefault
  , eventTargetBlur
  )
where

import           Control.Monad
import           Data.HashMap.Strict           as HM
import           Data.Set                      as S
import           Web.Framework.Plzwrk.Base      ( dats
                                                , dats'
                                                , Attributes(..)
                                                )
import           Web.Framework.Plzwrk.Browserful

merge :: Attributes s opq -> Attributes s opq -> Attributes s opq
merge a b = MkAttributes { _style    = HM.union (_style a) (_style b)
                         , _class    = S.union (_class a) (_class b)
                         , _simple   = HM.union (_simple a) (_simple b)
                         , _handlers = HM.union (_handlers a) (_handlers b)
                         }

-- |Merges two 'Attributes'
(<.>)
  :: (s -> Attributes s opq)
  -> (s -> Attributes s opq)
  -> (s -> Attributes s opq)
a <.> b = (\s -> merge (a s) (b s))

-- |Constrcts a stateful 'Attributes' applicative functor from a single style.
wStyle :: String -> String -> (s -> Attributes s opq)
wStyle k v = (\s -> dats' { _style = HM.singleton k v })

-- |Constrcts an 'Attributes' from a single style.
wStyle' :: String -> String -> Attributes s opq
wStyle' k v = dats' { _style = HM.singleton k v }

-- |Constrcts a stateful 'Attributes' applicative functor from a list of styles.
wStyles :: [(String, String)] -> (s -> Attributes s opq)
wStyles kvs = (\s -> dats' { _style = HM.fromList kvs })

-- |Constrcts an 'Attributes' from a list of styles.
wStyles' :: [(String, String)] -> Attributes s opq
wStyles' kvs = dats' { _style = HM.fromList kvs }

-- |Constrcts a stateful 'Attributes' applicative functor from a single class.
wClass :: String -> (s -> Attributes s opq)
wClass k = (\s -> dats' { _class = S.singleton k })

-- |Constrcts an 'Attributes' from a single class.
wClass' :: String -> Attributes s opq
wClass' k = dats' { _class = S.singleton k }

-- |Constrcts a stateful 'Attributes' applicative functor from a list of clases.
wClasses :: [String] -> (s -> Attributes s opq)
wClasses ks = (\s -> dats' { _class = S.fromList ks })

-- |Constrcts an 'Attributes' from a list of classes.
wClasses' :: [String] -> Attributes s opq
wClasses' ks = dats' { _class = S.fromList ks }

-- |Constrcts a stateful 'Attributes' applicative functor with a given id.
wId :: String -> (s -> Attributes s opq)
wId v = (\s -> dats' { _simple = HM.singleton "id" v })

-- |Constrcts an 'Attributes' with a given id.
wId' :: String -> Attributes s opq
wId' v = dats' { _simple = HM.singleton "id" v }

-- |Constrcts a stateful 'Attributes' applicative functor from an @onClick@ callback.
wOnClick :: (opq -> s -> IO s) -> (s -> Attributes s opq)
wOnClick v = (\s -> dats' { _handlers = HM.singleton "click" v })

-- |Constrcts an 'Attributes' from an @onClick@ callback.
wOnClick' :: (opq -> s -> IO s) -> Attributes s opq
wOnClick' v = dats' { _handlers = HM.singleton "click" v }

-- |Constrcts a stateful 'Attributes' applicative functor from an @onInput@ callback.
wOnInput :: (opq -> s -> IO s) -> (s -> Attributes s opq)
wOnInput v = (\s -> dats' { _handlers = HM.singleton "input" v })

-- |Constrcts an 'Attributes' from an @onInput@ callback.
wOnInput' :: (opq -> s -> IO s) -> Attributes s opq
wOnInput' v = dats' { _handlers = HM.singleton "input" v }

-- |Constrcts a stateful 'Attributes' applicative functor from a single attribute.
wAttr :: String -> String -> (s -> Attributes s opq)
wAttr k v = (\s -> dats' { _simple = HM.singleton k v })

-- |Constrcts an 'Attributes' from a single attribute.
wAttr' :: String -> String -> Attributes s opq
wAttr' k v = dats' { _simple = HM.singleton k v }

-- |Constrcts a stateful 'Attributes' applicative functor from a list of attributes.
wAttrs :: [(String, String)] -> (s -> Attributes s opq)
wAttrs kvs = (\s -> dats' { _simple = HM.fromList kvs })

-- |Constrcts an 'Attributes' from a list of attributes.
wAttrs' :: [(String, String)] -> Attributes s opq
wAttrs' kvs = dats' { _simple = HM.fromList kvs }

-----------------------------
---- events

-- |From an event, gets the target's value.
eventTargetValue :: Browserful jsval -> jsval -> IO (Maybe String)
eventTargetValue browser e = do
  opq <- (getPropertyAsOpaque browser) e "target"
  maybe (pure Nothing) (\y -> (getPropertyAsString browser) y "value") opq

-- |From an event, takes the target and blurs it.
eventTargetBlur :: Browserful jsval -> jsval -> IO ()
eventTargetBlur browser e = do
  opq <- (getPropertyAsOpaque browser) e "target"
  maybe (pure ()) (\y -> void $ (invokeOn0 browser) y "blur") opq

-- |Take an event and prevent the default.
eventPreventDefault :: Browserful jsval -> jsval -> IO ()
eventPreventDefault browser e = do
  void $ (invokeOn0 browser) e "preventDefault"
