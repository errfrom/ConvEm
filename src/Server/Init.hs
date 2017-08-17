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
import           Control.Monad                     (forever)
import           Server.Login                      (handleUser)
import           Server.General                    (initSocket, SocketType(..))
import           Data.ByteString           as BS   (head)


initServer :: IO ()
initServer = do
  sock <- initSocket ServerSocket
  forever (worker sock)
  where worker sock = do
          (conn, addr) <- Sock.accept sock
          Conc.forkIO $ forever (handleConn sock conn addr)

-- | Обрабатывает установленное с клиентом соединение.
-- В зависимости от полученного значения флага,
-- делегирует работу определенной функции,
-- знающей о контексте непосредственной обработки.
-- При выполнении, установленное соединение закрывается.
handleConn :: Sock.Socket -> Sock.Socket -> Sock.SockAddr -> IO ()
handleConn sock conn addr = do
  flag <- recv conn 1
  case flag of
    "1" -> handleUser conn >>= send conn >> return ()
    "0" -> Sock.close conn >>  Sock.close sock
    _   -> error "Undefined flag."
