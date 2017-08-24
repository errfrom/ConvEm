{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main.Login.Logic.General
  ( checkEmail, askServerResp ) where

--------------------------------------------------------------------------------
-- Функции, часто используемые в описании логики
-- входа пользователя в систему.
--------------------------------------------------------------------------------

import qualified Network.Socket            as Sock   (Socket(..))
import qualified Network.Socket.ByteString as SockBS (send, recv)
import qualified Data.ByteString           as BS     (singleton, append)
import Data.Word8                                    (_space)
import Types.Data                                    (UserData(..), RecoveryData'(..))
import Types.Results                                 (Results(..), AuthResult(..))
import Types.General                                 (FlagAssociated(..))
import Types.Server                                  (RequestType(..))

-- | Проверяет, может ли существовать подобный email.
-- Чем тщательнее выполняется проверка, тем меньше работы
-- предстоит сделать серверу для выявления несуществующих
-- E-mail адресов.
checkEmail :: String -> Bool
checkEmail email
 |isBlank email          = False
 |(not . elem '@') email = False -- Нет '@' в строке
 |(length . flip filter email) (== '@') /= 1 = False -- Несколько '@'
 |(not . elem '.' . afterEmailSymbol) email  = False -- Нет '.' после '@'
 |last email == '.' = False -- Заканчивается на '.'
 |otherwise         = True
 where afterEmailSymbol email = (tail . snd . flip break email) (== '@')
       isBlank value          = length value == 0

-- | Принимает входные данные какого-либо пользовательского действия,
-- ассоциирует с этим действия флаг и отсылает на сервер флаг, а затем
-- и сами данные объединенные в ByteString. Таким образом сервер знает,
-- какие данные ему предстоит обработать.
askServerResp :: (FlagAssociated a, Results a) => Sock.Socket -> UserData -> IO a
askServerResp sock AuthData{..} = do
  SockBS.send sock (toFlag Auth)
  _ <- SockBS.recv sock 1 -- Исскуственная блокировка.
  let data_ = foldl BS.append "" [ lEmail, BS.singleton _space, lPassword ]
  SockBS.send sock data_
  flag <- SockBS.recv sock 1
  return (toConstr flag)
askServerResp sock (RecoveryData (RDEmail email)) = do
  SockBS.send sock (toFlag Recovery)
  _ <- SockBS.recv sock 1
  SockBS.send sock email
  flag <- SockBS.recv sock 1
  return (toConstr flag)
