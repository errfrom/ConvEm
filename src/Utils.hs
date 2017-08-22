{-# LANGUAGE MultiParamTypeClasses #-}

module Utils
  ( removeClass, hasClass, getElemById, getElemType ) where

--------------------------------------------------------------------------------
-- Различные частоиспользуемые функции.
--------------------------------------------------------------------------------

import Graphics.UI.Threepenny.Core
import Text.Printf (printf)
import qualified Data.Maybe as M (fromJust)


-- | Удаляет определенный css-класс у элемента.
removeClass :: Element -> String -> UI()
removeClass element class' =
  let jsPattern = printf ".removeClass('%s')" class'
      jsFun     = ffi ("$(%1)" ++ jsPattern) element
  in runFunction jsFun

-- | Возвращает css-тип элемента.
getElemType :: Element -> UI String
getElemType el = callFunction (ffi "$(%1).attr('type')" el) >>= return

-- | Возвращает элемент по значению css-идентификатора.
getElemById :: String -> UI Element
getElemById id' = do
  window <- askWindow
  mElem  <- getElementById window id'
  return (M.fromJust mElem)

-- | Указывает, имеет ли елемент тот или иной css-класс.
hasClass :: Element -> String -> UI Bool
hasClass el class' =
  let jsPattern = printf ".hasClass('%s')" class'
  in callFunction (flip ffi el $ "$(%1)" ++ jsPattern) >>= (return . jsBool)
  where jsBool val
         | val == "true" = True
         | otherwise     = False
