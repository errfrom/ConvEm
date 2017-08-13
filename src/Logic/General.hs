module Logic.General
  (Email, Password, HashedPassword
  ,MistakeIn(..)) where

import Data.ByteString (ByteString(..))

type Email          = String
type Password       = String
type HashedPassword = ByteString

-- | Уточняет, в каком именно
-- поле совершена ошибка ввода.
-- Тем самым, дает дополнительную
-- информацию Frontend-логике.
data MistakeIn =
  InEmailField
 |InPasswordField
 |InBothFields
 deriving (Show)
