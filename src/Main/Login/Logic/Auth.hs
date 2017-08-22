module Main.Login.Logic.Auth
  ( auth ) where

import Graphics.UI.Threepenny.Core (get, value)
import Network.Socket              (Socket)
import Data.ByteString.Char8       (pack)
import Main.Login.Logic.General    (checkEmail)
-- TODO

--------------------------------------------------------------------------------
-- Backend часть процесса авторизации на стороне клиента.
--------------------------------------------------------------------------------


-- | Реализация Backend-части авторизации с
-- результатом, значение которого впоследствие
-- передается Frontend-логике.
auth :: Socket -> Element -> Element -> UI LoginResult
auth sock inpEmail inpPassw = do
  email <- getValue inpEmail
  passw <- getValue inpPassw
  res   <- liftIO $ worker email passw
  return res
  where getValue = get value
        worker email passw =
          case (checkReceivedValues email passw) of
            Just mistakeIn -> return (InvalidValues mistakeIn)
            Nothing        -> receive sock (LoginData (pack email) (pack passw))

-- | Проверяет, могут ли существовать
-- введенные пользователем значения.
checkReceivedValues :: String -> String -> Maybe MistakeIn
checkReceivedValues email password
 |checkPassword password !&& checkEmail email = Just InBothFields
 |(not . checkEmail) email       = Just InEmailField
 |(not . checkPassword) password = Just InPasswordField
 |otherwise                      = Nothing
  where (!&&) a b     = (not a) && (not b)
        isBlank value = length value == 0
        checkPassword = (not . isBlank)
