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

-- |Attributes for a DOM Node.
-- Attributes are parameterized by two types
-- * @s@ - the type of the state
-- * @opq@ - the type of an opaque object in JavaScript
--
-- You will rarely need to instantiate @Attributes@ yourself,
-- as it is easier to work with utility functions like 'wId',
-- 'wStyle' etc that produce Applicative Functors with signature
-- @(s -> Attributes s opq)@. These AFs are used in the 'Node' data.
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

-- | A DOM node.
-- The easiest way to create nodes is using tags such as
-- 'Web.Framework.Plzwrk.Util.span' or 'Web.Framework.Plzwrk.br'.
-- Nodes can be created for arbitrary tags using the 'Element'
-- constructor.
--
-- Node is parameterized by two types
-- * @s@ - the type of the state
-- * @opq@ - the type of an opaque object in JavaScript
--
-- Note that nodes, when passed as an arguemnt to 'plzwrk', need
-- to be Applicative Functors in the form @(s -> Node s opq)@.
data Node s opq = Element
    { _elt_tag :: String
    , _elt_attrs :: (s -> Attributes s opq)
    , _elt_children :: [s -> Node s opq]
    } | TextNode { _tn_text :: String }

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
