module Web.Framework.Plzwrk.Base
  ( hydrate
  , dats
  , dats'
  , PwNode(..)
  , HydratedPwNode(..)
  , PwAttribute(..)
  , cssToStyle
  , toHTML
  )
where

import           Data.List

import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as S

cssToStyle :: (HM.HashMap String String) -> String
cssToStyle css =
  (intercalate ";" $ fmap (\(x, y) -> x <> ":" <> y) (HM.toList css))


-- |PwAttribute for a DOM PwNode.

-- PwAttribute are parameterized by two types

-- * @s@ - the type of the state

-- * @opq@ - the type of an opaque object in JavaScript

--

-- You will rarely need to instantiate @PwAttribute@ yourself,

-- as it is easier to work with utility functions like 'wId',

-- 'wStyle' etc that produce Applicative Functors with signature

-- @(s -> PwAttribute s opq)@. These AFs are used in the 'PwNode' data.

data PwAttribute s opq = PwTextAttribute String | PwFunctionAttribute (opq -> s -> IO s)

dats = []
dats' = []

instance Show (PwAttribute s opq) where
  show (PwTextAttribute     t) = "PwTextAttribute (" <> t <> ")"
  show (PwFunctionAttribute t) = "PwFunctionAttribute"

-- |A DOM node.
-- The easiest way to create nodes is using tags such as
-- 'Web.Framework.Plzwrk.Util.span' or 'Web.Framework.Plzwrk.br'.
-- PwNodes can be created for arbitrary tags using the 'PwElement'
-- constructor.
--
-- PwNode is parameterized by two types
-- * @s@ - the type of the state
-- * @opq@ - the type of an opaque object in JavaScript
--
-- Note that nodes, when passed as an arguemnt to 'plzwrk', need
-- to be Applicative Functors in the form @(s -> PwNode s opq)@.

data PwNode s opq = PwElement
    { _elt_tag :: String
    , _elt_attrs :: [(String, (s -> PwAttribute s opq))]
    , _elt_children :: [s -> PwNode s opq]
    } | PwTextNode { _tn_text :: String }

instance Show (PwNode s opq) where
  show (PwElement t _ _) = show t
  show (PwTextNode t   ) = show t

data HydratedPwNode s opq = HydratedPwElement
    { _hy_tag  :: String
    , _hy_attr :: [(String, PwAttribute s opq)]
    , _hy_kids :: [HydratedPwNode s opq]
    }
    | HydratedPwTextNode String
    deriving (Show)

_hydrate :: s -> PwNode s opq -> HydratedPwNode s opq
_hydrate s (PwElement a b c) =
  HydratedPwElement a (fmap (\(x, y) -> (x, y s)) b) (fmap (\x -> hydrate s x) c)
_hydrate s (PwTextNode t) = HydratedPwTextNode t

hydrate :: s -> (s -> PwNode s opq) -> HydratedPwNode s opq
hydrate s f = _hydrate s (f s)

stringifyPwAttribute :: PwAttribute state jsval -> String
stringifyPwAttribute (PwTextAttribute     t) = t
stringifyPwAttribute (PwFunctionAttribute _) = ""

isText :: PwAttribute state jsval -> Bool
isText (PwTextAttribute _) = True
isText _                   = False

_toHTML :: HydratedPwNode state jsval -> String
_toHTML (HydratedPwElement tag attrs ch) =
  "<"
    ++ tag
    ++ (if (null atts) then "" else " " ++ atts)
    ++ (if (null ch)
         then "/>"
         else ">" ++ (concat $ fmap _toHTML ch) ++ "</" ++ tag ++ ">"
       )
 where
  atts = intercalate " " $ fmap
    (\(x, y) -> x <> "=\"" <> stringifyPwAttribute y <> "\"")
    (filter (isText . snd) attrs)
_toHTML (HydratedPwTextNode txt) = txt

-- |Converts a PwNode to HTML.
toHTML
  :: (state -> PwNode state jsval) -- ^ A function that takes a state and produces a DOM
  -> state -- ^ An initial state
  -> String -- ^ The resulting HTML
toHTML domF state = _toHTML (hydrate state domF)
