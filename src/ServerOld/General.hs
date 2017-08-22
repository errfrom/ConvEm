{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server.General
  ( SocketType(..), initSocket, receive ) where

--------------------------------------------------------------------------------
-- Дополнительный связующий слой между клиентом и сервером.
-- Включает в себя функции, преобразующие различные представления,
-- а также функции, выполняющие роль синтаксического сахара.
--------------------------------------------------------------------------------

import qualified Network.Socket            as Sock
import qualified Network.Socket.ByteString as SockBS (send, recv)
import qualified Data.ByteString           as BS     (singleton, append)
import           Data.ByteString                     (ByteString(..))
import           Logic.General                       (UserData(..), LoginResult(..))
import           Data.Word8
import           Types.General                       (FlagAssociated(..))


data SocketType =
   ServerSocket
  |ClientSocket

-- | Инициализирует сокет в зависимости
-- от переданного типа.
initSocket :: SocketType -> IO Sock.Socket
initSocket sockType =
  let tcpPrtcl  = 6
      localhost = Sock.tupleToHostAddress (127, 0, 0, 1) -- NOTE: Временное решение
      portNum   = 50000 -- TODO: Дополнительная проверка доступности номера порта.
      maxQueue  = 80 -- Сколько может находится соединений в очереди.
                     -- Предполагается, что операции происходят достаточно
                     -- быстро, т.к. делегируются другим потокам.
      socket'   = Sock.socket Sock.AF_INET Sock.Stream tcpPrtcl
      sockAddr  = Sock.SockAddrInet portNum localhost
  in case sockType of
       ServerSocket -> do
         sock <- socket'
         if (Sock.isSupportedSocketOption Sock.ReuseAddr)
           then Sock.setSocketOption sock Sock.ReuseAddr 1
           else return ()
         Sock.bind sock sockAddr
         Sock.listen sock maxQueue
         return sock
       ClientSocket -> do
         sock <- socket'
         Sock.connect sock sockAddr
         return sock

receive :: Sock.Socket -> UserData -> IO LoginResult
receive sock LoginData{..} = do
  SockBS.send sock "1"
  _ <- SockBS.recv sock 1 -- Исскуственная блокировка.
  let data_ = foldl BS.append "" [ lEmail, BS.singleton _space, lPassword ]
  SockBS.send sock data_
  flag <- SockBS.recv sock 1
  return (toConstr flag :: LoginResult)
