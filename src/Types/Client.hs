{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module Types.Client
  ( HasCallFunction(..), CanBeHandled(..), Form(..) ) where

import Graphics.UI.Threepenny.Core (UI, Element)
import Network.Socket              (Socket)
import Types.Results               (Results)

class (Results r) => HasCallFunction t r where
  call :: t -> Socket -> [Element] -> UI r

class (HasCallFunction t r) => CanBeHandled t r where
  handle :: t -> Socket -> UI ()

class (CanBeHandled t r) => Form t r where
  form :: t -> Socket -> UI Element
