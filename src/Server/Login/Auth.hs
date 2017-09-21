{-# LANGUAGE OverloadedStrings #-}

module Server.Login.Auth
  ( handleAuthorization ) where

import Network.Socket            (Socket)
import Network.Socket.ByteString (recv, send)
import Data.ByteString           (ByteString)
import Data.Word8                (_space)
import Database.MySQL.Simple     (Only(..))
import Login.Logic.Auth          (AuthResult(..))
import Types.General             (FlagAssociated(..))
import qualified Data.ByteString       as BS    (split)
import qualified Crypto.BCrypt         as Crypt (validatePassword)
import qualified Database.MySQL.Simple as MySql

handleAuthorization :: Socket -> IO ByteString
handleAuthorization conn = do
  _     <- send conn "1"
  data_ <- recv conn 200
  let email:passw:_ = BS.split _space data_
  mPasswHash <- getHashedPassword email
  let res = case mPasswHash of
              Nothing -> AuthInvalidData
              Just ph -> if (Crypt.validatePassword ph passw)
                           then AuthCorrectData else AuthInvalidData
  return (toFlag res)

-- Получает хеш пароля по указанному значению
-- поля email. Если пользователь отсутствует в базе,
-- возвращает Nothing.
getHashedPassword :: ByteString -> IO (Maybe ByteString)
getHashedPassword email =
  let query = "SELECT USERS_PASSWORD FROM USERS WHERE USER_EMAIL = ?"
  in do
    conn <- MySql.connect MySql.defaultConnectInfo
    sqlResult <- MySql.query conn query (Only email) :: IO [Only ByteString]
    return $ case sqlResult of
               [] -> Nothing
               [hashedPassword] -> Just $ fromOnly hashedPassword
