{-# LANGUAGE OverloadedStrings #-}

module Server.Login
  ( handleUser ) where

import qualified Network.Socket            as Sock
import           Network.Socket.ByteString          (recv, send)
import qualified Crypto.BCrypt             as Crypt (validatePassword)
import qualified Database.MySQL.Simple     as MySql (defaultConnectInfo, connect
                                                    ,close, query)
import           Database.MySQL.Simple              (ConnectInfo(..), Only(..))
import           Logic.General                      (LoginResult(..), Email
                                                    ,HashedPassword)
import qualified Data.ByteString           as BS    (singleton, split)
import           Data.ByteString                    (ByteString(..))
import           Data.Word8
import           Utils                              (FlagAssociated(..))

handleUser :: Sock.Socket -> IO ByteString
handleUser conn = do
  send conn "1"
  data_ <- recv conn 200
  let email:passw:_ = BS.split _space data_
  mPasswHash <- getHashedPassword email
  let res = case mPasswHash of
              Nothing -> NonexistentAccount
              Just ph -> validatePassword ph passw
  return (toFlag res)
  where validatePassword ph passw
         |Crypt.validatePassword ph passw == True = CorrectPassword
         |otherwise = IncorrectPassword

-- Получает хеш пароля по указанному значению
-- поля email. Если пользователь отсутствует в базе,
-- возвращает Nothing.
getHashedPassword :: Email -> IO (Maybe HashedPassword)
getHashedPassword email =
  let query = "SELECT USERS_PASSWORD FROM USERS WHERE USER_EMAIL = ?"
  in do
    conn <- MySql.connect MySql.defaultConnectInfo
    sqlResult <- MySql.query conn query (Only email) :: IO [Only HashedPassword]
    return $ case sqlResult of
               [] -> Nothing
               [hashedPassword] -> Just $ fromOnly hashedPassword
