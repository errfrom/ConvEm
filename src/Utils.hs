module Utils
  ( removeClass, checkEmail ) where

import           Text.Printf                                   (printf)
import           Graphics.UI.Threepenny.Core
--import qualified Graphics.UI.Threepenny.Internal as UIInternal (fromJSObject)
--import           Foreign.JavaScript                            (JSObject(..))
import qualified Graphics.UI.Threepenny.Core as UIJS (runFunction, callFunction
                                                     ,ffi)

-- | Удаляет определенный класс у элемента
removeClass :: Element -> String -> UI()
removeClass element class' =
  let jsPattern = printf ".removeClass('%s')" class'
      jsFun     = UIJS.ffi ("$(%1)" ++ jsPattern) element
  in UIJS.runFunction jsFun

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

{-
getChildren :: Element -> UI [Element]
getChildren element =
  let jsFun = UIJS.ffi "$(%1).children()" element
  in do
    jsObjects <- UIJS.callFunction jsFun
    return (map UIInternal.fromJSObject jsObjects) -}
