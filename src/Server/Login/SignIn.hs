{-# LANGUAGE OverloadedStrings #-}

module Server.Login.SignIn
  ( handleAuthorization ) where

import Network.Socket            (Socket)
import Network.Socket.ByteString (recv, send)
import Data.ByteString           (ByteString)
import Database.MySQL.Simple     (Only(..))
import Login.Types               (SignInDatum(..), SignInResult(..))
import Types.ServerAction        (constrAsFlag)
import qualified Data.Binary           as Bin   (decode)
import qualified Data.ByteString.Lazy  as LBS   (fromStrict)
import qualified Crypto.BCrypt         as Crypt (validatePassword)
import qualified Database.MySQL.Simple as MySql

handleAuthorization :: Socket -> IO ByteString
handleAuthorization conn = do
  _      <- send conn "1"
  bsData <- LBS.fromStrict <$> recv conn 200
  let data_ = Bin.decode bsData :: SignInDatum
  mPasswHash <- getHashedPassword (signInEmail data_)
  let res = case mPasswHash of
              Nothing -> SignInInvalidData
              Just ph -> if (Crypt.validatePassword ph $ signInPassw data_)
                           then SignInAuthorized else SignInInvalidData
  return (constrAsFlag res)

-- Получает хеш пароля по указанному значению
-- поля email. Если пользователь отсутствует в базе,
-- возвращает Nothing.
getHashedPassword :: ByteString -> IO (Maybe ByteString)
getHashedPassword email =
  let query = "SELECT USERS_PASSWORD FROM USERS WHERE USER_EMAIL = ?"
  in do
    conn <- MySql.connect MySql.defaultConnectInfo {MySql.connectDatabase = "users"
                                                   ,MySql.connectPassword = "adimro8010"}
    sqlResult <- MySql.query conn query (Only email) :: IO [Only ByteString]
    return $ case sqlResult of
               [] -> Nothing
               [hashedPassword] -> Just $ fromOnly hashedPassword
