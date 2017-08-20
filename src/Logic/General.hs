{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.General where

import           Data.ByteString         (ByteString(..))
import           Data.ByteString.Char8   (pack)
import           Utils                   (FlagAssociated(..))
import           Sugar.GenFlagAssociated (deriveFlagAssociated)

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
   InvalidValues MistakeIn
  |CorrectPassword
  |IncorrectPassword
  |NonexistentAccount
  |BlockedAccount
$(deriveFlagAssociated "LoginResult")

data UserData =
  LoginData { lEmail    :: Email
            , lPassword :: HashedPassword }
