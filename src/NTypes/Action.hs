{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

module NTypes.Action
  (doAction
  ,Action(..), ActionData
  ,AuthData(..)) where

--------------------------------------------------------------------------------
-- Описывает модель вводимых пользователем данных.
--------------------------------------------------------------------------------

import Graphics.UI.Threepenny (UI)
import Data.ByteString.Char8  (ByteString)
import Data.String            (IsString)
import NTypes.Result          (IsResult, Result(..))


doAction :: (Action a p r) => a -> UI (Result p r)
doAction a = do
  userInp  <- getUserInp a
  return $ case (checkUserInp userInp) of
    Just preResult -> PreResult preResult
    Nothing        -> (ServerResponse . sendServer . prepareData) userInp

-- Action Classes --------------------------------------------------------------

-- Характеризует тип посылаемых данных.
class () ActionData t where

class (ActionData a, IsResult p, IsResult r) => Action a p r | a -> p r where
  getUserInp   :: a -> UI a
  checkUserInp :: a -> Maybe p
  prepareData  :: a -> a
  sendServer   :: a -> r

-- Action Data -----------------------------------------------------------------

-- TODO: Добавить тип авторизации при помощи мобильного телефона.
data AuthData a =
  AuthData { authEmail :: ByteString
           , authPassw :: a }

instance Functor AuthData where
  fmap fun d@(AuthData{ authPassw = insecurePassw }) =
    d { authPassw = fun insecurePassw }

-- Action Instances ------------------------------------------------------------

instance (IsString a) => ActionData (AuthData a) where
