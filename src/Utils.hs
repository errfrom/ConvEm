module Utils
  ( removeClass ) where

import           Text.Printf                                (printf)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Core       as UIJS  (runFunction, ffi)

-- | Удаляет определенный класс у элемента
removeClass :: Element -> String -> UI()
removeClass element class' =
  let jsPattern = printf ".removeClass('%s')" class'
      jsFun     = UIJS.ffi ("$(%1)" ++ jsPattern) element
  in UIJS.runFunction jsFun
