{-# LANGUAGE ExistentialQuantification #-}

module Types.Frames
  ( FrameClass(..), AnyFrame(..) ) where

----------------------------------------------------------------------------------------------------
-- Любое представление UI состоит из структур (frames).
-- Структура характеризется элементами, которые она содержат.
-- Структура инициализируется определенными данными. (Data -> initFrame -> Frame)
-- Структуры, образующие представление, независимы друг от друга.
--
-- TODO : * Функции изменения состояния струтктуры.
--        * Функции удаления структуры.
--        * Удобная обработка списков элементов.
--        * Небольшой DSL для удобного описания DOM-иерархий.
----------------------------------------------------------------------------------------------------

import Data.Typeable                       (Typeable)
import System.Glib.UTFString               (GlibString)
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.Element  (Element)

class (Typeable frame, Traversable frame) => FrameClass frame where
  initFrame    :: (DocumentClass doc, GlibString builder) => doc -> frame builder -> IO (frame Element)

data AnyFrame = forall frame. FrameClass frame => AnyFrame
  { unAnyFrame :: frame Element }
