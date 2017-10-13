{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.General
  (LoginStage(..)
  ,LoginPrimaryData(..), defaultPut) where

--------------------------------------------------------------------------------
-- Объявления, часто использующиеся в других модулях.
--------------------------------------------------------------------------------

import GHC.Generics                 (Generic)
import Data.Foldable                (toList)
import Data.Default                 (Default, def)
import Data.Data                    (Data)
import Data.Binary                  (Binary(..), Put, Get)
import qualified Data.Binary as Bin (putList, get)

-- Stages Description ----------------------------------------------------------

data LoginStage =
    SignInStage
  | SignUpStage
  | RecoveryStageEmail
  | RecoveryStageKey
  | RecoveryStageChangePassw
  deriving (Data, Show)

instance Default LoginStage where
  def = SignInStage

-- Data related ----------------------------------------------------------------

data LoginPrimaryData a =
  LoginPrimaryData { email :: a
                   , passw :: a }
  deriving (Show, Foldable, Generic)

instance (Binary a) => Binary (LoginPrimaryData a) where
  put = defaultPut
  get = (Bin.get :: Get [a]) >>= \(e:p:_) -> return (LoginPrimaryData e p)

defaultPut :: (Binary a, Foldable t) => t a -> Put
defaultPut = Bin.putList . toList
