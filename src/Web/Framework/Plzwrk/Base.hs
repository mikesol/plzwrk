module Web.Framework.Plzwrk.Base
  ( hydrate
  , dats
  , dats'
  , PwNode(..)
  , HydratedPwNode(..)
  , PwAttributes(..)
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


-- |PwAttributes for a DOM PwNode.
-- PwAttributes are parameterized by two types
-- * @s@ - the type of the state
-- * @opq@ - the type of an opaque object in JavaScript
--
-- You will rarely need to instantiate @PwAttributes@ yourself,
-- as it is easier to work with utility functions like 'wId',
-- 'wStyle' etc that produce Applicative Functors with signature
-- @(s -> PwAttributes s opq)@. These AFs are used in the 'PwNode' data.
data PwAttributes s opq = MkPwAttributes
  { _style    :: HM.HashMap String String
  , _class    :: S.Set String
  , _simple   :: HM.HashMap String String
  , _handlers :: HM.HashMap String (opq -> s -> IO s)
  }

dats = (\_ -> MkPwAttributes HM.empty S.empty HM.empty HM.empty)
dats' = MkPwAttributes HM.empty S.empty HM.empty HM.empty


instance Show (PwAttributes s opq) where
  show (MkPwAttributes __style __class __simple _) =
    "PwAttributes ("
      <> show __style
      <> ", "
      <> show __class
      <> ", "
      <> show __simple
      <> ")"

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
    , _elt_attrs :: (s -> PwAttributes s opq)
    , _elt_children :: [s -> PwNode s opq]
    } | PwTextNode { _tn_text :: String }

instance Show (PwNode s opq) where
  show (PwElement t _ _) = show t
  show (PwTextNode t   ) = show t

data HydratedPwNode s opq = HydratedPwElement
    { _hy_tag  :: String
    , _hy_attr :: (PwAttributes s opq)
    , _hy_kids :: [HydratedPwNode s opq]
    }
    | HydratedPwTextNode String
    deriving (Show)

_hydrate :: s -> PwNode s opq -> HydratedPwNode s opq
_hydrate s (PwElement a b c) =
  HydratedPwElement a (b s) (fmap (\x -> hydrate s x) c)
_hydrate s (PwTextNode t) = HydratedPwTextNode t

hydrate :: s -> (s -> PwNode s opq) -> HydratedPwNode s opq
hydrate s f = _hydrate s (f s)

stringifyPwAttributes :: PwAttributes state jsval -> String
stringifyPwAttributes (MkPwAttributes __style __class __simple _) =
  intercalate " " $ filter
    (not . null)
    [ (if (HM.null __style)
        then ""
        else "style=\"" ++ cssToStyle __style ++ "\""
      )
    , (if (S.null __class)
        then ""
        else "class=\"" ++ unwords (S.toList __class) ++ "\""
      )
    , (if (HM.null __simple)
        then ""
        else intercalate " "
          $ fmap (\(x, y) -> x ++ "=\"" ++ y ++ "\"") (HM.toList __simple)
      )
    ]

_toHTML :: HydratedPwNode state jsval -> String
_toHTML (HydratedPwElement tag attrs ch) =
  "<"
    ++ tag
    ++ (if (null atts) then "" else " " ++ atts)
    ++ (if (null ch)
         then "/>"
         else ">" ++ (concat $ fmap _toHTML ch) ++ "</" ++ tag ++ ">"
       )
  where atts = stringifyPwAttributes attrs
_toHTML (HydratedPwTextNode txt) = txt

-- |Converts a PwNode to HTML.
toHTML
  :: (state -> PwNode state jsval) -- ^ A function that takes a state and produces a DOM
  -> state -- ^ An initial state
  -> String -- ^ The resulting HTML
toHTML domF state = _toHTML (hydrate state domF)
