{-|
Module      : Web.Framework.Plzwrk
Description : Base functions for plzwrk
Copyright   : (c) Mike Solomon 2020
License     : GPL-3
Maintainer  : mike@meeshkan.com
Stability   : experimental
Portability : POSIX, Windows

This module contains the base functions for plzwrk, notably
the family of plzwrk functions needed for plzwrk to work. It
also exports most of the utility functions used for building
web applications, like event handling and attribute wrangling.
-}

module Web.Framework.Plzwrk
  ( hydrate
  , dats
  , dats'
  , Node(..)
  , HydratedNode(..)
  , Attributes(..)
  , cssToStyle
  , Browserful(..)
  , reconcile
  , plzwrk
  , plzwrk'
  , plzwrk'_
  , OldStuff(..)
  )
where

-- need to hide div from prelude

import           Prelude                 hiding ( div )
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.Browserful
import           Web.Framework.Plzwrk.Domify
