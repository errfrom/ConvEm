module Main.Login.Logic.Auth
  ( auth ) where

import Graphics.UI.Threepenny.Core (UI(..), Element, liftIO)
import Network.Socket              (Socket)
import Data.ByteString.Char8       (pack)
import Main.Login.Logic.General    (checkEmail, askServerResp)
import Types.Results               (MistakeIn(..), AuthResult(..))
import Types.Data                  (UserData(..))
import Utils                       (getValue)

--------------------------------------------------------------------------------
-- Backend часть процесса авторизации на стороне клиента.
--------------------------------------------------------------------------------


-- | Реализация Backend-части авторизации с
-- результатом, значение которого впоследствие
-- передается Frontend-логике.
auth :: Socket -> Element -> Element -> UI AuthResult
auth sock inpEmail inpPassw = do
  email <- getValue inpEmail
  passw <- getValue inpPassw
  res   <- liftIO $ worker sock email passw
  return res
  where worker sock email passw =
          case (checkReceivedValues email passw) of
            Just mistakeIn -> return (InvalidValues mistakeIn)
            Nothing        ->
              let loginData = AuthData (pack email) (pack passw)
              in  (askServerResp sock loginData :: IO AuthResult)

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
