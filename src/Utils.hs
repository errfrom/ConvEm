module Utils where

import           Text.Printf                                   (printf)
import           Graphics.UI.Threepenny.Core
--import qualified Graphics.UI.Threepenny.Internal as UIInternal (fromJSObject)
--import           Foreign.JavaScript                            (JSObject(..))
import qualified Graphics.UI.Threepenny.Core as UIJS (runFunction, callFunction
                                                     ,ffi)
import           Data.ByteString                     (ByteString(..))
import           Sugar.GenFlagAssociated             (genFlagAssocInstance)


-- | Удаляет определенный класс у элемента
removeClass :: Element -> String -> UI()
removeClass element class' =
  let jsPattern = printf ".removeClass('%s')" class'
      jsFun     = UIJS.ffi ("$(%1)" ++ jsPattern) element
  in UIJS.runFunction jsFun

getElemType :: Element -> UI String
getElemType el = UIJS.callFunction (UIJS.ffi "$(%1).attr('type')" el) >>= return

-- | Проверяет, может ли существовать подобный email.
checkEmail email
 |isBlank email          = False
 |(not . elem '@') email = False -- Нет '@' в строке
 |(length . flip filter email) (== '@') /= 1 = False -- Несколько '@'
 |(not . elem '.' . afterEmailSymbol) email  = False -- Нет '.' после '@'
 |last email == '.' = False -- Заканчивается на '.'
 |otherwise         = True
 where afterEmailSymbol email = (tail . snd . flip break email) (== '@')
       isBlank value          = length value == 0

--------------------------------------------------------------------------------
data A = A | B | C

$(genFlagAssocInstance "A")

type Flag = String

class FlagAssociated t where
  toFlag  :: t -> Flag
  toField :: Flag -> t
