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
  ( plzwrk
  , plzwrk'
  , plzwrk'_
  , plzwrkSSR
  , toHTML
  , PwNode(..)
  , PwAttribute(..)
  , JSEnv(..)
  -- pwx
  , pwx
  , pwx'
  , plusplus
  , parsePWX
  , parsePWX_
  , PWX(..)
  , PWXAttribute(..)
  -- util
  , pF
  , pT
  , eventPreventDefault
  , eventTargetBlur
  , eventTargetValue
  , elementSetAttribute
  , elementTagName
  , eventTargetAddEventListener
  , eventTargetRemoveEventListener
  , getPropertyAsBool
  , getPropertyAsDouble
  , getPropertyAsInt
  , getPropertyAsString
  , htmlElemenetClick
  , consoleLogS
  , nodeAppendChild
  , nodeChildNodes
  , nodeInsertBefore
  , nodeRemoveChild
  , nodeTextContent
  )
where

-- need to hide div from prelude

import           Prelude                 hiding ( div )
import           Web.Framework.Plzwrk.Base
import           Web.Framework.Plzwrk.JSEnv
import           Web.Framework.Plzwrk.Domify
import           Web.Framework.Plzwrk.Util
import           Web.Framework.Plzwrk.TH.PWX
import           Web.Framework.Plzwrk.TH.QuotePWX
