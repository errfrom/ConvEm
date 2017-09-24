{-# LANGUAGE AutoDeriveTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}

module Types.General
  (Stage(..), Chainable
  ,constrAsFlag, flagAsConstr
  ,toSingleChain) where

--------------------------------------------------------------------------------
-- Объявления, часто использующиеся в других модулях.
--------------------------------------------------------------------------------

import Data.Monoid                             ((<>))
import Data.Default                            (Default, def)
import Data.ByteString.Char8                   (ByteString)
import Data.Data                               (Data, ConIndex)
import qualified Data.ByteString.Char8 as BS8  (pack, unpack)
import qualified Data.Data             as Data

type Flag = ByteString

data Stage = Auth | Reg
  deriving (Data)

instance Default Stage where
  def = Auth

-- Функции, ассоциирующие конструкторы типа с флагом. --------------------------

constrAsFlag :: (Data t) => t -> Flag
constrAsFlag = BS8.pack . show . Data.constrIndex . Data.toConstr

flagAsConstr :: forall proxy t. (Default t, Data t) => Flag -> proxy t -> t
flagAsConstr flag _ =
  let dt    = Data.dataTypeOf (def :: t)
      flag' = read (BS8.unpack flag) :: ConIndex
  -- NOTE: indexConstr является небезопасной из-за функции '!!'.
  --       Было принято решение не обработывать возможное исключение с целью
  --       не использовать монаду IO. Исключение врядли возникнет в связи с тем,
  --       что эта функция используется в паре с constrAsFlag.
  in (Data.fromConstr . Data.indexConstr dt) flag'

-- Chainable интерфейс. --------------------------------------------------------

type Chainable t a = (Foldable t, Monoid a)

toSingleChain :: (Chainable t a) => a -> t a -> a
toSingleChain delimiter = foldMap (\x -> x <> delimiter)
