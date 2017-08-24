module Types.General
  ( FlagAssociated(..)
  , RecStage(..), Stage(..)
  , HashedPassword ) where

--------------------------------------------------------------------------------
-- Набор различной дополнительной/общей информации,
-- потенциально использующейся различными модулями.
--------------------------------------------------------------------------------

import Data.ByteString.Char8 (ByteString(..))


type Flag           = ByteString
type HashedPassword = ByteString

-- Экземпляры этого класса ассоциируются с определенными флагами,
-- передающимися к серверу и обратно. Использование функций
-- toFlag и toConstr на порядок безопасней из-за подразумевающейся
-- синхронизации. Какие бы значения флагов не были сгенерированы
-- для экземпляра, результат всегда будет положительный.
-- Дабы избавиться от написания шаблонного кода, в модуле
-- Templates/GenFlagAssociated.hs описана логика генератора экземпляров
-- класса.
class FlagAssociated t where
  toFlag   :: t -> Flag
  toConstr :: Flag -> t

data RecStage =
  SendingEmail
 |SendingKey
 |ChangePassword

data Stage =
  Start
 |Auth
 |Reg
 |Recovery RecStage
