{-# LANGUAGE OverloadedStrings #-}

module Server.Init
  ( initServer ) where

--------------------------------------------------------------------------------
-- Прототип кода, выполняющегося на сервере.
-- Создает сокет в локальной сети.
--------------------------------------------------------------------------------

import qualified Network.Socket            as Sock
import           Network.Socket.ByteString         (recv, send)
import qualified Control.Concurrent        as Conc (forkIO, forkFinally)
import           Server.Login                      (handleLogin)
import           Data.ByteString           as BS   (unpack)
import           Data.Word8


initServer :: IO ()
initServer = Sock.withSocketsDo $
  let tcpPrtcl  = 6
      localhost = Sock.tupleToHostAddress (127, 0, 0, 1) -- NOTE: Временное решение
      portNum   = 3500 -- TODO: Дополнительная проверка доступности номера порта.
      maxQueue  = 80 -- Сколько может находится соединений в очереди.
                     -- Предполагается, что операции происходят достаточно
                     -- быстро, т.к. делегируются другим потокам.
  in do
    sock <- Sock.socket Sock.AF_INET Sock.Stream tcpPrtcl
    Sock.bind sock (Sock.SockAddrInet portNum localhost)
    Sock.listen sock maxQueue
    worker sock
  where worker sock = do
          (conn, addr) <- Sock.accept sock
          _ <- Conc.forkIO (handleConn conn addr)
          worker sock

-- | Обрабатывает установленное с клиентом соединение.
-- В зависимости от полученного значения флага,
-- делегирует работу определенной функции,
-- знающей о контексте непосредственной обработки.
-- При выполнении, установленное соединение закрывается.
handleConn :: Sock.Socket -> Sock.SockAddr -> IO ()
handleConn conn addr = do
  flag <- recv conn 1
  case (BS.unpack flag) of
    _1 -> handleUser conn >>= send conn
  Sock.close conn
