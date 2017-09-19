{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.General
  (FlagAssociated(..), Stage(..)) where

--------------------------------------------------------------------------------
-- Объявления, часто использующиеся в других модулях.
--------------------------------------------------------------------------------

import Data.ByteString.Char8       (ByteString, pack)
import Templates.GenFlagAssociated (deriveFlagAssociated)

type Flag = ByteString

-- Ассоциация конструкторов типа t с определенными флагами.
-- Можно автоматически создать экземпляр с помощью функции
-- deriveFlagAssociated.
class FlagAssociated t where
  toFlag   :: t -> Flag
  toConstr :: Flag -> t

data Stage = Auth | Reg
$(deriveFlagAssociated [ "Stage" ])
