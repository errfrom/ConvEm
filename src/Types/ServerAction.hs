{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.ServerAction
  (ServerActionData(..), ServerAction(..)
  ,serverRequest
  ,constrAsFlag, flagAsConstr) where

--------------------------------------------------------------------------------
-- Абстракция уровня общения клиента с сервером.
-- Интерфейс ServerAction определяет функцию runServerAction,
-- инксапсулирующую определенное обращение клиента к серверу.
--------------------------------------------------------------------------------

import Control.Exception (throwIO)
import GHC.IO.Exception

import Data.Data                               (Data, ConIndex)
import Data.Proxy                              (Proxy(..))
import Data.Binary                             (Binary)
import Data.ByteString.Char8                   (ByteString)
import Data.ByteString.Lazy.Char8              (toStrict)
import Data.Monoid                             ((<>))
import Network.Socket                          (Socket)
import Network.Socket.ByteString               (recv, send)
import Types.General                           (LoginStage)
import qualified Data.Binary           as Bin  (encode)
import qualified Data.ByteString.Char8 as BS8  (pack, unpack)
import qualified Data.Data             as Data (dataTypeOf, toConstr, fromConstr
                                               ,constrIndex, indexConstr)

-- Эта функция переводит все необходимое в бинарные данные и в нужном
-- порядке отсылает серверу.
serverRequest :: (ServerAction a) => LoginStage -> Socket -> a -> IO (ServerActionResult a)
serverRequest stage sock actionData =
  let actionDataEncoded = (toStrict . Bin.encode) actionData
      dataToSend        = constrAsFlag stage <> actionDataEncoded
  in do
    _ <- send sock dataToSend
    -- _ <- throwIO (IOError Nothing IllegalOperation "" "" Nothing Nothing) NOTE: To produce neterr.
    flagResult <- recv sock 1
    return $ flagAsConstr flagResult (Proxy :: Proxy (ServerActionResult a))

-- Action Interface ------------------------------------------------------------

class (Data d, Binary d) => ServerActionData d where
  validateData :: d -> Bool
  validateData _ = True

class (ServerActionData d, Data (ServerActionResult d)) => ServerAction d where
  data ServerActionResult d :: *
  runServerAction :: Socket -> d -> IO (ServerActionResult d)

-- Функции, ассоциирующие конструкторы типа с флагом. --------------------------

type Flag = ByteString

constrAsFlag :: (Data t) => t -> Flag
constrAsFlag = BS8.pack . show . Data.constrIndex . Data.toConstr

flagAsConstr :: forall proxy t. (Data t) => Flag -> proxy t -> t
flagAsConstr flag _ =
  let dt    = Data.dataTypeOf (undefined :: t)
      flag' = read (BS8.unpack flag) :: ConIndex
  -- NOTE: indexConstr является небезопасной из-за функции '!!'.
  --       Было принято решение не обработывать возможное исключение с целью
  --       не использовать монаду IO. Исключение врядли возникнет в связи с тем,
  --       что эта функция используется в паре с constrAsFlag.
  in (Data.fromConstr . Data.indexConstr dt) flag'
