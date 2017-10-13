{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server.General
  (SocketType(..)
  ,initServer, handleConn, initSocket) where

--------------------------------------------------------------------------------
-- Содержит частоиспользуемые в других модулях Server/ функции.
-- Часть функций выступает в роли абстрактного слоя, позволяющего
-- писать меньше шаблонного кода.
--------------------------------------------------------------------------------

import Data.Proxy                           (Proxy(..))
import Network.Socket                       (Socket)
import Network.Socket.ByteString            (recv, send)
import Control.Monad                        (forever)
import Types.General                        (LoginStage(..))
import Types.ServerAction                   (flagAsConstr)
import Server.Login.SignIn                  (handleAuthorization)
import qualified Network.Socket     as Sock
import qualified Control.Concurrent as Conc (forkIO)

data SocketType = ClientSocket | ServerSocket

initSocket :: SocketType -> IO Socket
initSocket sockType =
  let tcpPrtcl  = 6
      localhost = Sock.tupleToHostAddress (127, 0, 0, 1)
      portNum   = 50000 -- TODO: Дополнительная проверка доступности номера порта.
      maxQueue  = 80
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
handleConn :: Socket -> Socket -> IO ()
handleConn _ conn = do
  flagStage <- recv conn 1
  case (flagAsConstr flagStage (Proxy :: Proxy LoginStage)) of
    SignInStage -> handleAuthorization conn >>= answerClient
  where answerClient res = send conn res >> return ()
