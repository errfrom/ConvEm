module Utils
  ( FlagAssociated(..)
  , checkEmail
  , removeClass, getElemType ) where

import           Graphics.UI.Threepenny.Core
import           Text.Printf                  (printf)
import           Data.ByteString              (ByteString(..))

-- | Удаляет определенный css-класс у элемента
removeClass :: Element -> String -> UI()
removeClass element class' =
  let jsPattern = printf ".removeClass('%s')" class'
      jsFun     = ffi ("$(%1)" ++ jsPattern) element
  in runFunction jsFun

getElemType :: Element -> UI String
getElemType el = callFunction (ffi "$(%1).attr('type')" el) >>= return

-- | Проверяет, может ли существовать подобный email.
-- Чем тщательнее выполняется проверка, тем меньше работы
-- предстоит сделать серверу для выявления несуществующих
-- E-mail адресов.
checkEmail email
 |isBlank email          = False
 |(not . elem '@') email = False -- Нет '@' в строке
 |(length . flip filter email) (== '@') /= 1 = False -- Несколько '@'
 |(not . elem '.' . afterEmailSymbol) email  = False -- Нет '.' после '@'
 |last email == '.' = False -- Заканчивается на '.'
 |otherwise         = True
 where afterEmailSymbol email = (tail . snd . flip break email) (== '@')
       isBlank value          = length value == 0

type Flag = ByteString

-- Экземпляры этого класса ассоциируются с определенными флагами,
-- передающимися к серверу и обратно. Использование функций
-- toFlag и toField на порядок безопасней из-за подразумевающейся
-- синхронизации. Какие бы значения флагов не были сгенерированы
-- для экземпляра, результат всегда будет положительный.
-- Дабы избавиться от написания шаблонного кода, в модуле
-- Sugar/GenFlagAssociated.hs описана логика генератора экземпляров
-- класса.
class FlagAssociated t where
  toFlag  :: t -> Flag
  toField :: Flag -> t
