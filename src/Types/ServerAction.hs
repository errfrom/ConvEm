{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types.ServerAction
  (ServerActionData(..), ServerActionResult, ServerAction(..)
  ,serverRequest) where

--------------------------------------------------------------------------------
-- Абстракция уровня общения клиента с сервером.
-- Интерфейс ServerAction определяет функцию doAction,
-- инксапсулирующую определенное обращение клиента к серверу.
--------------------------------------------------------------------------------

import Control.Monad             (void)
import Network.Socket            (Socket)
import Network.Socket.ByteString (recv, send)
import Data.ByteString.Char8     (ByteString)
import Types.General             (FlagAssociated(..), Stage)

serverRequest :: (ServerAction d r) => Stage -> Socket -> d -> IO r
serverRequest stage sock actionData = do
  _ <- send sock (toFlag stage)
  waitServer
  _ <- send sock (asSingleBS actionData)
  flagResult <- recv sock 1
  return (toConstr flagResult)
  where waitServer = void (recv sock 1)

-- Action Interface ------------------------------------------------------------

class ServerActionData d where -- TODO: С помощью getFields сделать стандартную имплементацию.
  asSingleBS :: d -> ByteString
  validate   :: d -> Bool

class (FlagAssociated r) => ServerActionResult r where

class (ServerActionData d, ServerActionResult r) => ServerAction d r | d -> r where
  runServerAction :: Socket -> d -> IO r
