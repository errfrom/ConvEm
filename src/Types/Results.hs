{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Results
  ( MistakeIn(..)
  , AuthResult(..), RecoveryResult(..), Result ) where

--------------------------------------------------------------------------------
-- Типы данных, описывающие всевозможные результаты
-- различных событий.
--------------------------------------------------------------------------------

import Data.ByteString.Char8       (pack)
import Types.General               (FlagAssociated(..))
import Templates.GenFlagAssociated (deriveFlagAssociated)


class    Result t              where
instance Result AuthResult     where
instance Result RecoveryResult where

-- | Указывает, в каком поле ввода совершена ошибка.
data MistakeIn =
  InEmailField
 |InPasswordField

-- | Результат авторизации.
data AuthResult =
  InvalidValues [MistakeIn]
 |CorrectPassword
 |IncorrectPassword
 |ANonexistentAccount
 |BlockedAccount

-- | Результат процедуры восстановления пароля.
data RecoveryResult =
  InvalidEmail -- <=> InvalidValues InEmailField
 |RNonexistentAccount
 |RecMailSent
 |SendingDenied
 |BadKey
 |WrongKey
 |RightKey

$(deriveFlagAssociated [ "AuthResult"
                       , "RecoveryResult" ])
