{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Types.Frames
  ( FrameClass(..), AnyFrame(..), ReleaseFunction(..)
  , ElementUpdatable(..), createChildElement, appendChildren, createContainer ) where

----------------------------------------------------------------------------------------------------
-- Любое представление UI состоит из структур (frames).
-- Структура характеризется элементами, которые она содержат.
-- Структура инициализируется определенными данными. (Data -> initFrame -> Frame)
-- Структуры, образующие представление, независимы друг от друга.
----------------------------------------------------------------------------------------------------

import Data.Typeable                                (Typeable, typeOf, typeRepFingerprint)
import System.Glib.UTFString                        (GlibString)
import Data.Text                                    (Text)
import Graphics.UI.Gtk.WebKit.DOM.Document          (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.Element           (ElementClass, Element, toElement)
import Graphics.UI.Gtk.WebKit.DOM.HTMLButtonElement (HTMLButtonElementClass)
import Graphics.UI.Gtk.WebKit.DOM.Node              (NodeClass)
import Graphics.Data.Selectors                      (CSSSel, unSel)
import qualified Data.Maybe                             as M       (fromJust)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element     as Element (setAttribute)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLElement as Element (setInnerText)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document    as Doc     (createElement)
import qualified Graphics.UI.Gtk.WebKit.DOM.Node        as Node    (appendChild)

newtype ReleaseFunction = ReleaseFunction { runReleaser :: IO () }


class (Typeable frame, Traversable frame) => FrameClass frame where
  initFrame :: (DocumentClass doc) => doc -> frame Text -> IO (ReleaseFunction, frame Element)

data AnyFrame = forall frame. FrameClass frame => AnyFrame
  { unAnyFrame :: frame Element }

instance Eq AnyFrame where
  a == b = worker a == worker b
    where worker = typeRepFingerprint . typeOf

instance Ord AnyFrame where
  a <= b = worker a <= worker b
    where worker = typeRepFingerprint . typeOf

-- Elements and related 'sugar' functions ----------------------------------------------------------

class (ElementClass element) => ElementUpdatable element datum | element -> datum where
  updateElement :: element -> datum -> IO Element

instance (HTMLButtonElementClass element) => ElementUpdatable element Text where
  updateElement element text = Element.setInnerText element (Just text) >> return (toElement element)

createChildElement :: (DocumentClass doc, NodeClass p, GlibString t) => doc -> p -> t -> IO Element
createChildElement doc parent tag = do
  element <- Doc.createElement doc (Just tag)
  _ <- Node.appendChild parent element
  return (M.fromJust element)

appendChildren :: (NodeClass parent, NodeClass child) => parent -> [child] -> IO ()
appendChildren parent = mapM_ (Node.appendChild parent . Just)

createContainer :: (DocumentClass doc, NodeClass parent, NodeClass child) => doc
                                                                          -> parent
                                                                          -> CSSSel
                                                                          -> [child]
                                                                          -> IO Element
createContainer doc parent containerId children = do
  container <- createChildElement doc parent ("div" :: Text)
  Element.setAttribute container "id" (unSel containerId)
  appendChildren container children
  return container
