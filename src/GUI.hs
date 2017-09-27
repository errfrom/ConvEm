module GUI
  (elCalled, removeClass, addClass, hasClass
  ,getValue) where

import Control.Exception                  (Exception, throw)
import Text.Printf                        (printf)
import Graphics.UI.Threepenny.Core 

type Id = String

newtype UnidentifiableElementException = NonexistentId Id
instance Exception UnidentifiableElementException where

instance Show UnidentifiableElementException where
  show (NonexistentId id') = "Element with id - " ++ id' ++ " not found."

-- Functions -------------------------------------------------------------------

elCalled :: Id -> UI Element
elCalled id' = do
  win       <- askWindow
  mElement  <- getElementById win id'
  case mElement of
    Nothing -> throw (NonexistentId id')
    Just el -> return el

-- Удаляет определенный css-класс у элемента.
removeClass :: Element -> String -> UI ()
removeClass el class' =
  let jsPattern = printf ".removeClass('%s')" class'
      jsFun     = ffi ("$(%1)" ++ jsPattern) el
  in runFunction jsFun

-- Добавляет определенный css-класс элементу.
addClass :: Element -> String -> UI ()
addClass el class' =
  let jsPattern = printf ".addClass('%s')" class'
      jsFun     = ffi ("$(%1)" ++ jsPattern) el
  in runFunction jsFun

-- Указывает, имеет ли елемент тот или иной css-класс.
hasClass :: Element -> String -> UI Bool
hasClass el class' =
  let jsPattern = printf ".hasClass('%s')" class'
  in callFunction (flip ffi el $ "$(%1)" ++ jsPattern) >>= (return . jsBool)
  where jsBool :: String -> Bool
        jsBool val
         | val == "true" = True
         | otherwise     = False

getValue :: Element -> UI String
getValue = get value
