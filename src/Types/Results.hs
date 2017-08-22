{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Types.Results
  ( MistakeIn(..)
  , AuthResult, RecoveryResult ) where

--------------------------------------------------------------------------------
-- Типы данных, описывающие всевозможные результаты
-- различных событий.
--------------------------------------------------------------------------------

import qualified Data.ByteString.Char8 (pack)
import Types.General                   (FlagAssociated(..))
import Templates.GenFlagAssociated     (deriveFlagAssociated)


-- | Указывает, в каком поле ввода совершена ошибка.
-- Тем самым, дает дополнительную информацию Frontend-логике.
data MistakeIn =
  InEmailField
 |InPasswordField
 |InBothFields

-- | Результат авторизации.
data AuthResult =
  InvalidValues MistakeIn
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

$(deriveFlagAssociated [ "AuthResult"
                       , "RecoveryResult" ])
