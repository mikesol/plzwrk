{-# LANGUAGE OverloadedStrings #-}
module Web.Framework.Plzwrk.Base
  ( div
  , div'
  , div_
  , div'_
  , txt
  , txt'
  , input
  , input'
  , input_
  , input'_
  , button
  , button'
  , button_
  , button'_
  , p
  , p'
  , p_
  , p'_
  , hydrate
  , dats
  , dats'
  , Node(..)
  , HydratedNode(..)
  , Attributes(..)
  , cssToStyle
  )
where

import           Data.HashMap.Strict
import           Data.Set                hiding ( empty
                                                , toList
                                                )
import           Data.Text               hiding ( empty )
import           Prelude                 hiding ( String
                                                , concat
                                                , div
                                                )

cssToStyle :: (HashMap Text Text) -> Text
cssToStyle css =
  (pack "{")
    <> (intercalate (pack ";") $ fmap (\(x, y) -> x <> ":" <> y) (toList css))
    <> (pack "}")

-- data classes

data Attributes s opq = MkAttributes
  { _css     :: Maybe (HashMap Text Text)
  , _class   :: Maybe (Set Text)
  , _simple  :: HashMap Text Text
  , _handlers :: HashMap Text (opq -> s -> IO s)
  }

dats = (\_ -> MkAttributes Nothing Nothing empty empty)
dats' = MkAttributes Nothing Nothing empty empty


instance Show (Attributes s opq) where
  show (MkAttributes __css __class __simple _) =
    "Attributes ("
      <> show __css
      <> ", "
      <> show __class
      <> ", "
      <> show __simple
      <> ")"

data Node s opq = Element Text (s -> Attributes s opq) [s -> Node s opq]
    | TextNode Text

instance Show (Node s opq) where
  show (Element t _ _) = show t
  show (TextNode t   ) = show t

type AFSig s opq
  = (s -> Attributes s opq) -> [s -> Node s opq] -> (s -> Node s opq)
type Sig s opq = (s -> Attributes s opq) -> [s -> Node s opq] -> Node s opq

type AFSig_ s opq = [s -> Node s opq] -> (s -> Node s opq)
type Sig_ s opq = [s -> Node s opq] -> Node s opq

-- div
div :: AFSig s opq
div x y = (\_ -> Element "div" x y)

div' :: Sig s opq
div' = Element "div"

div_ :: AFSig_ s opq
div_ x = (\_ -> Element "div" dats x)

div'_ :: Sig_ s opq
div'_ x = Element "div" dats x

-- button
button :: AFSig s opq
button x y = (\_ -> Element "button" x y)

button' :: Sig s opq
button' = Element "button"

button_ :: AFSig_ s opq
button_ x = (\_ -> Element "button" dats x)

button'_ :: Sig_ s opq
button'_ x = Element "button" dats x

-- input
input :: AFSig s opq
input x y = (\_ -> Element "input" x y)

input' :: Sig s opq
input' = Element "input"

input_ :: AFSig_ s opq
input_ x = (\_ -> Element "input" dats x)

input'_ :: Sig_ s opq
input'_ x = Element "input" dats x

-- p
p :: AFSig s opq
p x y = (\_ -> Element "p" x y)

p' :: Sig s opq
p' = Element "p"

p_ :: AFSig_ s opq
p_ x = (\_ -> Element "p" dats x)

p'_ :: Sig_ s opq
p'_ x = Element "p" dats x

txt :: Text -> (s -> Node s opq)
txt t = (\_ -> TextNode t)

txt' :: Text -> Node s opq
txt' = TextNode

-- for hydration


data HydratedNode s opq = HydratedElement
    { _hy_tag  :: Text
    , _hy_attr :: (Attributes s opq)
    , _hy_kids :: [HydratedNode s opq]
    }
    | HydratedTextNode Text
    deriving (Show)

_hydrate :: s -> Node s opq -> HydratedNode s opq
_hydrate s (Element a b c) =
  HydratedElement a (b s) (fmap (\x -> hydrate s x) c)
_hydrate s (TextNode t) = HydratedTextNode t

hydrate :: s -> (s -> Node s opq) -> HydratedNode s opq
hydrate s f = _hydrate s (f s)
