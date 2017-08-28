{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server.General
  ( initServer, handleConn, initSocket ) where

--------------------------------------------------------------------------------
-- Содержит частоиспользуемые в других модулях Server/ функции.
-- Часть функций выступает в роли абстрактного слоя, позволяющего
-- писать меньше шаблонного кода.
--------------------------------------------------------------------------------

import qualified Network.Socket     as Sock
import Network.Socket.ByteString            (recv, send)
import qualified Control.Concurrent as Conc (forkIO)
import Control.Monad                        (forever)
import Types.General                        (FlagAssociated(..)
                                            ,SocketType(..), RequestType(..))
import Server.Login.Auth                    (handleUser)
--import Server.Login.Recovery                (handleReceiver)


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
         putStrLn "GOT."
         sock <- socket'
         Sock.connect sock sockAddr
         return sock

initServer :: IO ()
initServer = do
  sock <- initSocket ServerSocket
  forever (worker sock)
  where worker sock = do
          (conn, _) <- Sock.accept sock
          Conc.forkIO $ forever (handleConn sock conn)

-- | Обрабатывает установленное с клиентом соединение.
-- В зависимости от полученного значения флага,
-- делегирует работу определенной функции,
-- знающей о контексте непосредственной обработки.
-- При выполнении, установленное соединение закрывается.
handleConn :: Sock.Socket -> Sock.Socket -> IO ()
handleConn sock conn = do
  flag <- recv conn 1
  case (toConstr flag :: RequestType) of
    Auth     -> handleUser conn     >>= answerClient
--    Recovery -> handleReceiver conn >>= answerClient
    Exit     -> Sock.close conn >>  Sock.close sock
    _        -> error "Undefined flag."
  where answerClient res = send conn res >> return ()
