{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ConstraintKinds        #-}

module Types.ServerAction
  (ServerActionData(..), ServerActionResult, ServerAction(..)
  ,serverRequest
  ,constrAsFlag, flagAsConstr) where

--------------------------------------------------------------------------------
-- Абстракция уровня общения клиента с сервером.
-- Интерфейс ServerAction определяет функцию runServerAction,
-- инксапсулирующую определенное обращение клиента к серверу.
--------------------------------------------------------------------------------

import System.IO.Error                         (IOError)
import Control.Monad                           (void)
import Control.Monad.Except                    (ExceptT, liftIO)
import Data.Default                            (Default, def)
import Data.Data                               (Data, ConIndex)
import Data.Proxy                              (Proxy(..))
import Data.Binary                             (Binary)
import Data.ByteString.Char8                   (ByteString)
import Network.Socket                          (Socket)
import Network.Socket.ByteString               (recv, send)
import Types.General                           (LoginStage)
import Data.ByteString.Lazy.Char8              (toStrict)
import qualified Data.Binary           as Bin  (encode)
import qualified Data.ByteString.Char8 as BS8  (pack, unpack)
import qualified Data.Data             as Data (dataTypeOf, toConstr, fromConstr
                                               ,constrIndex, indexConstr)

-- Эта функция переводит все необходимое в бинарные данные и в нужном
-- порядке отсылает серверу.
serverRequest :: forall proxy r. forall a d. (Binary a, ServerAction (d a) r)
              => LoginStage -> Socket -> d a -> proxy r -> IO r --ExceptT IOError IO r
serverRequest stage sock actionData _ = do
  _ <- (send sock $ constrAsFlag stage)
  liftIO waitServer
  _ <- (send sock . toStrict . Bin.encode $ actionData)
  flagResult <- (recv sock 1)
  return $ flagAsConstr flagResult (Proxy :: Proxy r)
  where waitServer = void (recv sock 1)

-- Action Interface ------------------------------------------------------------

class (Binary d) => ServerActionData d where
  validateData :: d -> Bool
  validateData _ = True

class (FlagConstrAssociative r) => ServerActionResult r where

class ( ServerActionData   d
      , ServerActionResult r ) => ServerAction d r | d -> r where
  runServerAction :: Socket -> d -> IO r -- ExceptT IOError IO r

-- Функции, ассоциирующие конструкторы типа с флагом. --------------------------

type Flag                    = ByteString
type FlagConstrAssociative t = (Data t, Default t)

constrAsFlag :: (Data t) => t -> Flag
constrAsFlag = BS8.pack . show . Data.constrIndex . Data.toConstr

flagAsConstr :: forall proxy t. (FlagConstrAssociative t) => Flag -> proxy t -> t
flagAsConstr flag _ =
  let dt    = Data.dataTypeOf (def :: t)
      flag' = read (BS8.unpack flag) :: ConIndex
  -- NOTE: indexConstr является небезопасной из-за функции '!!'.
  --       Было принято решение не обработывать возможное исключение с целью
  --       не использовать монаду IO. Исключение врядли возникнет в связи с тем,
  --       что эта функция используется в паре с constrAsFlag.
  in (Data.fromConstr . Data.indexConstr dt) flag'
