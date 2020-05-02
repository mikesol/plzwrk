module Web.Framework.Plzwrk.Base
  ( hydrate
  , dats
  , dats'
  , Node(..)
  , HydratedNode(..)
  , Attributes(..)
  , cssToStyle
  )
where

import           Data.List

import           Data.HashMap.Strict
import           Data.Set                hiding ( empty
                                                , toList
                                                )
import qualified Data.Set                      as S

cssToStyle :: (HashMap String String) -> String
cssToStyle css =
  (intercalate ";" $ fmap (\(x, y) -> x <> ":" <> y) (toList css))

-- data classes

data Attributes s opq = MkAttributes
  { _style    :: HashMap String String
  , _class    :: Set String
  , _simple   :: HashMap String String
  , _handlers :: HashMap String (opq -> s -> IO s)
  }

dats = (\_ -> MkAttributes empty S.empty empty empty)
dats' = MkAttributes empty S.empty empty empty


instance Show (Attributes s opq) where
  show (MkAttributes __style __class __simple _) =
    "Attributes ("
      <> show __style
      <> ", "
      <> show __class
      <> ", "
      <> show __simple
      <> ")"

data Node s opq = Element String (s -> Attributes s opq) [s -> Node s opq]
    | TextNode String

instance Show (Node s opq) where
  show (Element t _ _) = show t
  show (TextNode t   ) = show t

data HydratedNode s opq = HydratedElement
    { _hy_tag  :: String
    , _hy_attr :: (Attributes s opq)
    , _hy_kids :: [HydratedNode s opq]
    }
    | HydratedTextNode String
    deriving (Show)

_hydrate :: s -> Node s opq -> HydratedNode s opq
_hydrate s (Element a b c) =
  HydratedElement a (b s) (fmap (\x -> hydrate s x) c)
_hydrate s (TextNode t) = HydratedTextNode t

hydrate :: s -> (s -> Node s opq) -> HydratedNode s opq
hydrate s f = _hydrate s (f s)
