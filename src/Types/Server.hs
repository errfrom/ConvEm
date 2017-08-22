{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Types.Server
  ( SocketType(..), RequestType(..) ) where

--------------------------------------------------------------------------------
-- Содержит определения данных, используемых на сервере.
--------------------------------------------------------------------------------

import Templates.GenFlagAssociated (deriveFlagAssociated)
import Data.ByteString.Char8       (pack)
import Types.General               (FlagAssociated(..))

data SocketType =
   ServerSocket
  |ClientSocket

data RequestType =
   Auth
  |Reg
  |Recovery
  |Exit

$(deriveFlagAssociated [ "RequestType" ])
