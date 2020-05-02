module Web.Framework.Plzwrk.Util
  ( (@=)
  , (<.>)
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
  , getTargetValue
  , preventDefault
  , blurTarget
  )
where

import           Data.HashMap.Strict           as HM
import           Data.Set                      as S
import           Web.Framework.Plzwrk.Base      ( dats
                                                , dats'
                                                , Attributes(..)
                                                )
import           Web.Framework.Plzwrk.Browserful

(@=) :: k -> v -> (k, v)
k @= v = (k, v)

merge :: Attributes s opq -> Attributes s opq -> Attributes s opq
merge a b = MkAttributes { _style    = HM.union (_style a) (_style b)
                         , _class    = S.union (_class a) (_class b)
                         , _simple   = HM.union (_simple a) (_simple b)
                         , _handlers = HM.union (_handlers a) (_handlers b)
                         }

(<.>)
  :: (s -> Attributes s opq)
  -> (s -> Attributes s opq)
  -> (s -> Attributes s opq)
a <.> b = (\s -> merge (a s) (b s))

wStyle :: String -> String -> (s -> Attributes s opq)
wStyle k v = (\s -> dats' { _style = HM.singleton k v })

wStyle' :: String -> String -> Attributes s opq
wStyle' k v = dats' { _style = HM.singleton k v }

wStyles :: [(String, String)] -> (s -> Attributes s opq)
wStyles kvs = (\s -> dats' { _style = HM.fromList kvs })

wStyles' :: [(String, String)] -> Attributes s opq
wStyles' kvs = dats' { _style = HM.fromList kvs }

wClass :: String -> (s -> Attributes s opq)
wClass k = (\s -> dats' { _class = S.singleton k })

wClass' :: String -> Attributes s opq
wClass' k = dats' { _class = S.singleton k }

wClasses :: [String] -> (s -> Attributes s opq)
wClasses ks = (\s -> dats' { _class = S.fromList ks })

wClasses' :: [String] -> Attributes s opq
wClasses' ks = dats' { _class = S.fromList ks }

wId :: String -> (s -> Attributes s opq)
wId v = (\s -> dats' { _simple = HM.singleton "id" v })

wId' :: String -> Attributes s opq
wId' v = dats' { _simple = HM.singleton "id" v }

wOnClick :: (opq -> s -> IO s) -> (s -> Attributes s opq)
wOnClick v = (\s -> dats' { _handlers = HM.singleton "click" v })

wOnClick' :: (opq -> s -> IO s) -> Attributes s opq
wOnClick' v = dats' { _handlers = HM.singleton "click" v }

wOnInput :: (opq -> s -> IO s) -> (s -> Attributes s opq)
wOnInput v = (\s -> dats' { _handlers = HM.singleton "input" v })

wOnInput' :: (opq -> s -> IO s) -> Attributes s opq
wOnInput' v = dats' { _handlers = HM.singleton "input" v }

wAttr :: String -> String -> (s -> Attributes s opq)
wAttr k v = (\s -> dats' { _simple = HM.singleton k v })

wAttr' :: String -> String -> Attributes s opq
wAttr' k v = dats' { _simple = HM.singleton k v }

wAttrs :: [(String, String)] -> (s -> Attributes s opq)
wAttrs kvs = (\s -> dats' { _simple = HM.fromList kvs })

wAttrs' :: [(String, String)] -> Attributes s opq
wAttrs' kvs = dats' { _simple = HM.fromList kvs }

-----------------------------
---- events

getTargetValue :: Browserful jsval -> jsval -> IO (Maybe String)
getTargetValue browser e = do
  opq <- (getOpaque browser) e "target"
  maybe (pure Nothing) (\y -> (getString browser) y "value") opq

blurTarget :: Browserful jsval -> jsval -> IO ()
blurTarget browser e = do
  opq <- (getOpaque browser) e "target"
  maybe (pure ()) (\y -> (invokeOn browser) y "blur") opq

preventDefault :: Browserful jsval -> jsval -> IO ()
preventDefault browser e = do
  (invokeOn browser) e "preventDefault"
