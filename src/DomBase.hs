{-# LANGUAGE OverloadedStrings #-}
module DomBase
    ( div, txt, ul, li, ol,
    hydrate, dats, Node(..), HydratedNode(..),
     Attributes(..), cssToStyle, ClickEvent(..), MouseUpEvent(..),
     CSS(..)
    ) where

import           Data.Set
import           Data.Text hiding (empty)
import           Prelude   hiding (String, concat, div)

data ClickEvent = ClickEvent
data MouseUpEvent = MouseUpEvent
data CSS = CSS
    { _position :: Maybe Text
    }
    deriving (Show, Eq)

cssToStyle :: CSS -> Text
cssToStyle css = "{ " <> (maybe "" ((<>) "position: ") $ _position css) <> " }"

-- data classes

data Attributes s = MkAttributes
    { _css     :: Maybe CSS
    , _class   :: Maybe (Set Text)
    , _simple  :: Set (Text, Text)
    , _onClick :: Maybe (ClickEvent -> s)
    }

dats = (\_ -> MkAttributes Nothing Nothing empty Nothing)

instance Show (Attributes s) where
    show (MkAttributes __css __class __simple _) = "Attributes (" <> show __css <> ", " <> show __class <> ", " <> show __simple <> ")"

data Node s = Element Text (s -> Attributes s) [s -> Node s]
    | TextNode Text

data HydratedNode s = HydratedElement Text (Attributes s) [HydratedNode s]
    | HydratedTextNode Text
    deriving (Show)

_hydrate :: s -> Node s -> HydratedNode s
_hydrate s (Element a b c) = HydratedElement a (b s) (fmap (\x -> hydrate s x) c)
_hydrate s (TextNode t) = HydratedTextNode t

hydrate :: s -> (s -> Node s) -> HydratedNode s
hydrate s f = _hydrate s (f s)

instance Show (Node s) where
    show (Element t _ _) = show t
    show (TextNode t)    = show t

div :: (s -> Attributes s) -> [s -> Node s] -> Node s
div = Element "div"

ul :: (s -> Attributes s) -> [s -> Node s] -> Node s
ul = Element "ul"

ol :: (s -> Attributes s) -> [s -> Node s] -> Node s
ol = Element "ol"

li :: (s -> Attributes s) -> [s -> Node s] -> Node s
li = Element "li"

p :: (s -> Attributes s) -> [s -> Node s] -> Node s
p = Element "p"

txt :: Text -> Node s
txt = TextNode
