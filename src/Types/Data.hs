module Types.Data
  ( UserData(..), RecoveryData'(..) ) where

--------------------------------------------------------------------------------
-- Различные данные, формирующиеся на основе пользовательского
-- взаимодействия с программой.
--------------------------------------------------------------------------------

import Data.ByteString.Char8 (ByteString)

data RecoveryData' =
  RDKey   ByteString
 |RDEmail ByteString
 |RDPassw ByteString

data UserData =
  AuthData  { lEmail    :: ByteString
            , lPassword :: ByteString }
 |RecoveryData RecoveryData'
