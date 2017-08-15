{-# LANGUAGE OverloadedStrings #-}

module Logic.Login
  ( MistakeIn(..)
  , LoginResult(..)
  , login ) where

--------------------------------------------------------------------------------
import           Graphics.UI.Threepenny.Core
import qualified Crypto.BCrypt           as Crypt  (validatePassword)
import           Data.ByteString.Char8             (ByteString(..), pack, unpack)
import qualified Data.Maybe              as M      (fromJust)
import           Control.Exception                 (catch)
import qualified System.Directory        as Dir    (getCurrentDirectory)
import           Utils                             (checkEmail)
import           Logic.General
--DB----------------------------------------------------------------------------
import qualified Database.MySQL.Simple as MySql
  (defaultConnectInfo, connect, close, insertID
  ,withTransaction, query)
import           Database.MySQL.Simple
  (ConnectInfo(..), Only(..))
--------------------------------------------------------------------------------

-- | Описывает результат запроса
-- пользователя на вход.
data LoginResult =
  InvalidValues MistakeIn -- Поля входа заполнены не правильно. (см. checkReceivedValues)
 |CorrectPassword
 |IncorrectPassword
 |NonexistentAccount
 |BlockedAccount -- TODO: Продумать логику. (Добавить поле в базу?)
 deriving (Show)

-- | Реализация Backend-части авторизации.
-- Возвращает LoginResult, тем самым
-- делегируя Frontend-логику модулю GUI.Login.
login :: Element -> Element -> UI LoginResult
login inpEmail inpPassw = do
  email    <- getValue inpEmail
  password <- getValue inpPassw
  worker email password
  where worker email password =
          case (checkReceivedValues email password) of
            Just mistakeIn -> return (InvalidValues mistakeIn)
            Nothing        -> do
              mPasswordHash <- liftIO (getHashedPassword email)
              return $
                case mPasswordHash of
                  Nothing           -> NonexistentAccount
                  Just passwordHash -> validatePassword passwordHash password
        getValue = get value
        validatePassword ph pd
         |Crypt.validatePassword (pack ph) (pack pd) == True = CorrectPassword
         |otherwise = IncorrectPassword

-- | Проверяет, могут ли существовать
-- введенные пользователем значения.
checkReceivedValues :: Email -> Password -> Maybe MistakeIn
checkReceivedValues email password
  |checkPassword password !&& checkEmail email = Just InBothFields
  |(not . checkEmail) email       = Just InEmailField
  |(not . checkPassword) password = Just InPasswordField
  |otherwise                      = Nothing
  where (!&&) a b = (not a) && (not b)
        isBlank value = length value == 0
        checkPassword = (not . isBlank)

-- Получает хеш пароля по указанному значению
-- поля email. Если пользователь отсутствует в базе,
-- возвращает Nothing.
getHashedPassword :: Email -> IO (Maybe Password)
getHashedPassword email =
  let query = "SELECT USERS_PASSWORD FROM USERS WHERE USER_EMAIL = ?"
  in do
    conn <- MySql.connect MySql.defaultConnectInfo -- NOTE: временное решение
    sqlResult <- MySql.query conn query (Only email) :: IO [Only HashedPassword]
    return $ case sqlResult of
               [] -> Nothing
               [hashedPassword] -> Just $ (unpack . fromOnly) hashedPassword
