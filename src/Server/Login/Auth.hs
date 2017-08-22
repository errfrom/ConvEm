{-# LANGUAGE OverloadedStrings #-}

module Server.Login.Auth
  ( handleUser ) where

import qualified Network.Socket        as Sock
import Network.Socket.ByteString                (recv, send)
import qualified Data.ByteString       as BS    (singleton, split)
import Data.ByteString                          (ByteString(..))
import Data.Word8                               (_space)
import qualified Crypto.BCrypt         as Crypt (validatePassword)
import qualified Database.MySQL.Simple as MySql
import Database.MySQL.Simple                    (ConnectInfo(..), Only(..))
import Types.Results                            (AuthResult(..))
import Types.General                            (HashedPassword, FlagAssociated(..))


handleUser :: Sock.Socket -> IO ByteString
handleUser conn = do
  send conn "1"
  data_ <- recv conn 200
  let email:passw:_ = BS.split _space data_
  mPasswHash <- getHashedPassword email
  let res = case mPasswHash of
              Nothing -> ANonexistentAccount
              Just ph -> validatePassword ph passw
  return (toFlag res)
  where validatePassword ph passw
         |Crypt.validatePassword ph passw == True = CorrectPassword
         |otherwise = IncorrectPassword

-- Получает хеш пароля по указанному значению
-- поля email. Если пользователь отсутствует в базе,
-- возвращает Nothing.
getHashedPassword :: ByteString -> IO (Maybe HashedPassword)
getHashedPassword email =
  let query = "SELECT USERS_PASSWORD FROM USERS WHERE USER_EMAIL = ?"
  in do
    conn <- MySql.connect MySql.defaultConnectInfo 
    sqlResult <- MySql.query conn query (Only email) :: IO [Only HashedPassword]
    return $ case sqlResult of
               [] -> Nothing
               [hashedPassword] -> Just $ fromOnly hashedPassword
