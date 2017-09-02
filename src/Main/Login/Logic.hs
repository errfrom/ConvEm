{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main.Login.Logic
  ( inpCheck, transformValue
  , askServerResp ) where

--------------------------------------------------------------------------------
-- Функции, часто используемые в описании логики
-- входа пользователя в систему.
--------------------------------------------------------------------------------

import Network.Socket                                (Socket)
import Crypto.BCrypt                                 (HashingPolicy(..))
import Data.ByteString                               (ByteString)
import Data.Word8                                    (_space)
import Data.Maybe                                    (fromJust)
import qualified Network.Socket.ByteString as SockBS (send, recv)
import qualified Data.ByteString           as BS     (singleton, append)
import qualified Crypto.BCrypt             as BCrypt (hashPasswordUsingPolicy
                                                     ,defaultHashAlgorithm)

import Types.Data    (UserData(..), RecoveryData'(..))
import Types.Results (Result)
import Types.Elems   (InputKind(..))
import Types.General (FlagAssociated(..), RequestType(..))


-- | Проверяет, введены ли корректные данные в поле определенного типа.
inpCheck :: InputKind -> String -> Bool
inpCheck (InpPassword _) passw
 |length passw == 0 = False
 |otherwise         = True
inpCheck (InpEmail _) email
 |isBlank email          = False
 |(not . elem '@') email = False -- Нет '@' в строке
 |(length . flip filter email) (== '@') /= 1 = False -- Несколько '@'
 |(not . elem '.' . afterEmailSymbol) email  = False -- Нет '.' после '@'
 |last email == '.' = False -- Заканчивается на '.'
 |otherwise         = True
  where afterEmailSymbol email' = (tail . snd . flip break email') (== '@')
        isBlank value          = length value == 0

-- | Определяет, следует ли предварительно изменять данные
-- перед отправкой на сервер и каким способом.
transformValue :: InputKind -> ByteString -> IO ByteString
transformValue (InpPassword _)  passw = do
  mHashed <- BCrypt.hashPasswordUsingPolicy
               (HashingPolicy 11 BCrypt.defaultHashAlgorithm) passw
  return (fromJust mHashed)

-- | Принимает входные данные какого-либо пользовательского действия,
-- ассоциирует с этим действия флаг и отсылает на сервер флаг, а затем
-- и сами данные объединенные в ByteString. Таким образом сервер знает,
-- какие данные ему предстоит обработать.
askServerResp :: (FlagAssociated a, Result a) => Socket -> UserData -> IO a
askServerResp sock AuthData{..} = do
  _ <- SockBS.send sock (toFlag Auth)
  _ <- SockBS.recv sock 1 -- Исскуственная блокировка.
  let data_ = foldl BS.append "" [ lEmail, BS.singleton _space, lPassword ]
  _ <- SockBS.send sock data_
  flag <- SockBS.recv sock 1
  return (toConstr flag)
askServerResp sock (RecoveryData (RDEmail email)) = do
  _ <- SockBS.send sock (toFlag Recovery)
  _ <- SockBS.recv sock 1
  _ <- SockBS.send sock email
  flag <- SockBS.recv sock 1
  return (toConstr flag)
