{-# LANGUAGE OverloadedStrings #-}
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

import           Data.HashMap.Strict
import           Data.Set                hiding ( empty
                                                , toList
                                                )
import qualified Data.Set                      as S
import           Data.Text               hiding ( empty )
import           Prelude                 hiding ( String
                                                , concat
                                                , div
                                                )

cssToStyle :: (HashMap Text Text) -> Text
cssToStyle css =
  (intercalate (pack ";") $ fmap (\(x, y) -> x <> ":" <> y) (toList css))

-- data classes

data Attributes s opq = MkAttributes
  { _style   :: HashMap Text Text
  , _class   :: Set Text
  , _simple  :: HashMap Text Text
  , _handlers :: HashMap Text (opq -> s -> IO s)
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

data Node s opq = Element Text (s -> Attributes s opq) [s -> Node s opq]
    | TextNode Text

instance Show (Node s opq) where
  show (Element t _ _) = show t
  show (TextNode t   ) = show t

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
