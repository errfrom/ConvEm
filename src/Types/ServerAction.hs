{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}

module Types.ServerAction
  (ServerActionData(..), ServerActionResult, ServerAction(..)
  ,serverRequest) where

--------------------------------------------------------------------------------
-- Абстракция уровня общения клиента с сервером.
-- Интерфейс ServerAction определяет функцию doAction,
-- инксапсулирующую определенное обращение клиента к серверу.
--------------------------------------------------------------------------------

import Control.Monad             (void)
import Data.Default              (Default)
import Data.Data                 (Data)
import Data.Proxy                (Proxy(..))
import Data.ByteString.Char8     (ByteString)
import Network.Socket            (Socket)
import Network.Socket.ByteString (recv, send)
import Types.General             (Chainable, Stage)
import qualified Types.General as General

serverRequest :: forall proxy r d. ( Chainable d ByteString
                                   , ServerActionData (d ByteString)
                                   , Default r, Data r
                                   , ServerAction (d ByteString) r )
              => Stage -> Socket -> d ByteString -> proxy r -> IO r
serverRequest stage sock actionData _ = do
  _ <- send sock (General.constrAsFlag stage)
  waitServer
  _ <- send sock (General.toSingleChain " " actionData)
  flagResult <- recv sock 1
  return $ General.flagAsConstr flagResult (Proxy :: Proxy r)
  where waitServer = void (recv sock 1)

-- Action Interface ------------------------------------------------------------

class ServerActionData d where -- TODO: С помощью getFields сделать стандартную имплементацию.
  validate   :: d -> Bool

class (Data r) => ServerActionResult r where

class (ServerActionData d, ServerActionResult r) => ServerAction d r | d -> r where
  runServerAction :: Socket -> d -> IO r
