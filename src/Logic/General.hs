module Logic.General
  (Email, HashedPassword
  ,MistakeIn(..), UserData(..), LoginResult(..)) where

import Data.ByteString (ByteString(..))

type Email          = ByteString
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

-- | Описывает результат запроса
-- пользователя на вход.
data LoginResult =
   InvalidValues MistakeIn -- Поля входа заполнены не правильно. (см. checkReceivedValues)
  |CorrectPassword
  |IncorrectPassword
  |NonexistentAccount
  |BlockedAccount -- TODO: Продумать логику. (Добавить поле в базу?)
  deriving (Show)

data UserData =
  LoginData { lEmail    :: Email
            , lPassword :: HashedPassword }
