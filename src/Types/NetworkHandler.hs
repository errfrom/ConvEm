{-# LANGUAGE DeriveFunctor #-}

module Types.NetworkHandler
  (NetworkHandler(..)) where

import qualified System.IO.Error  as IOErr    (tryIOError)
import qualified System.IO.Unsafe as IOUnsafe (unsafePerformIO)

data NetworkHandler a =
   NetHandlerResult a
  |NetHandlerAction (IO a)
  |NetworkError
  deriving (Functor)

-- NOTE: For debug.
instance (Show a) => Show (NetworkHandler a) where
  show (NetHandlerResult a) = "NetHandlerResult " ++ (show a)
  show (NetHandlerAction _) = "NetHandlerAction"
  show NetworkError         = "NetworkError"

-- Выполняет действие, учитывая возможные I/O ошибки.
executeNetworkAction :: IO a -> NetworkHandler a
{-# NOINLINE executeNetworkAction #-}
executeNetworkAction action = worker $
  (IOUnsafe.unsafePerformIO . IOErr.tryIOError) action
  where worker (Left _)    = NetworkError
        worker (Right res) = NetHandlerResult res

instance Applicative NetworkHandler where
  pure = NetHandlerResult

  -- Pure applicability
  (<*>) NetworkError _       = NetworkError
  (<*>) _ NetworkError       = NetworkError
  (<*>) (NetHandlerResult f)
        (NetHandlerResult r) = NetHandlerResult (f r)

  -- Impure applicability
  -- Все NetHandlerAction обязательно вычисляются в
  -- NetHandlerResult или NetworkError функцией 'executeNetworkAction'.
  (<*>) (NetHandlerResult f)
        (NetHandlerAction r) = NetHandlerResult f <*> executeNetworkAction r

  (<*>) (NetHandlerAction f)
        (NetHandlerAction r) = executeNetworkAction (f <*> r)

  (<*>) (NetHandlerAction f)
        (NetHandlerResult r) = executeNetworkAction (f <*> return r)

-- Абстракция цепочки действий, зависимых от состояния сетевого соединения.
instance Monad NetworkHandler where
  NetworkError       >>= _ = NetworkError
  NetHandlerResult x >>= k = k x
  NetHandlerAction x >>= k = executeNetworkAction x >>= k
