module Types.Data
  ( UserData(..) ) where

--------------------------------------------------------------------------------
-- Различные данные, формирующиеся на основе пользовательского
-- взаимодействия с программой.
--------------------------------------------------------------------------------

import Types.General         (HashedPassword)
import Data.ByteString.Char8 (ByteString(..))


data UserData =
  LoginData { lEmail    :: ByteString
            , lPassword :: HashedPassword }
