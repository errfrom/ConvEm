{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Server.Login.SignIn
  ( handleAuthorization ) where

import Data.ByteString           (ByteString)
import Data.Text                 (Text)
import Login.Types
import Types.ServerAction        (constrAsFlag)
import Types.DBEntities
import Database.Esqueleto
import qualified Data.Binary           as Bin   (decode)
import qualified Data.ByteString.Char8 as BS    (unpack)
import qualified Data.Text             as T     (pack)
import qualified Data.ByteString.Lazy  as LBS   (fromStrict)
import qualified Crypto.BCrypt         as Crypt (validatePassword)

handleAuthorization :: ByteString -> IO ByteString
handleAuthorization bsData =
  let data_ = (Bin.decode . LBS.fromStrict) bsData
  in do
    mPasswHash <- (getHashedPassword . T.pack . BS.unpack . signInEmail) data_
    let res = case mPasswHash of
                Nothing -> SignInInvalidData
                Just ph -> if (Crypt.validatePassword ph $ signInPassw data_)
                           then SignInAuthorized else SignInInvalidData
    return (constrAsFlag res)

-- Получает хеш пароля по указанному значению
-- поля email. Если пользователь отсутствует в базе,
-- возвращает Nothing.
getHashedPassword :: Text -> IO (Maybe ByteString)
getHashedPassword email = do
  userEntity <- worker email
  return $ case userEntity of []                  -> Nothing
                              ((Entity _ user):_) -> Just (uPassw user)
  where worker email = withDBConn . runSqlConn $ do
          select $
            from $ \user -> do
            where_ (user ^. UEmail ==. val email)
            distinct (return user)
