{-# LANGUAGE OverloadedStrings #-}

module Logic.Login
  ( MistakeIn(..)
  , login ) where

import           Network.Socket                   (Socket)
import           Graphics.UI.Threepenny.Core
import           Data.ByteString.Char8            (ByteString(..), pack, unpack)
import qualified Data.Maybe              as M     (fromJust)
import           Control.Exception                (catch)
import qualified System.Directory        as Dir   (getCurrentDirectory)
import           Utils                            (checkEmail)
import           Logic.General
import           Server.General


-- | Реализация Backend-части авторизации.
-- Возвращает LoginResult, тем самым
-- делегируя Frontend-логику модулю GUI.Login.
login :: Socket -> Element -> Element -> UI LoginResult
login sock inpEmail inpPassw = do
  email    <- getValue inpEmail
  password <- getValue inpPassw
  res      <- liftIO $ worker email password
  return res
  where getValue = get value
        worker email password =
          case (checkReceivedValues email password) of
            Just mistakeIn -> return (InvalidValues mistakeIn)
            Nothing        -> receive sock (LoginData (pack email) (pack password))

-- | Проверяет, могут ли существовать
-- введенные пользователем значения.
checkReceivedValues :: String -> String -> Maybe MistakeIn
checkReceivedValues email password
  |checkPassword password !&& checkEmail email = Just InBothFields
  |(not . checkEmail) email       = Just InEmailField
  |(not . checkPassword) password = Just InPasswordField
  |otherwise                      = Nothing
  where (!&&) a b = (not a) && (not b)
        isBlank value = length value == 0
        checkPassword = (not . isBlank)
