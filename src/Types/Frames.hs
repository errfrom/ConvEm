{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Types.Frames
  ( FrameClass(..), AnyFrame(..), ReleaseFunction(..)
  , ElementUpdatable(..)
  , createChildElement, appendChildren, createContainer
  , tokensToNodes, defaultFunRelease, asChildrenOf ) where

----------------------------------------------------------------------------------------------------
-- Любое представление UI состоит из структур (frames).
-- Структура характеризется элементами, которые она содержат.
-- Структура инициализируется определенными данными. (Data -> initFrame -> Frame)
-- Структуры, образующие представление, независимы друг от друга.
----------------------------------------------------------------------------------------------------

import Data.Typeable                                (Typeable, typeOf, typeRepFingerprint)
import System.Glib.UTFString                        (GlibString)
import Data.Text                                    (Text)
import Data.Tree                                    (Tree(..), Forest)
import Text.HTML.Parser                             (Token(..), Attr(..))
import Graphics.UI.Gtk.WebKit.DOM.Document          (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.Element           (ElementClass, Element, toElement)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement       (castToHTMLElement)
import Graphics.UI.Gtk.WebKit.DOM.HTMLButtonElement (HTMLButtonElementClass)
import Graphics.UI.Gtk.WebKit.DOM.Node              (NodeClass)
import Graphics.Data.Selectors                      (CSSSel, unSel)
import qualified Data.Maybe                             as M       (fromJust, catMaybes, isJust)
import qualified Data.Tree                              as Tree    (flatten)
import qualified Text.HTML.Tree                         as Tree    (tokensToForest)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element     as Element (setAttribute)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLElement as Element (setInnerText)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document    as Doc     (createElement)
import qualified Graphics.UI.Gtk.WebKit.DOM.Node        as Node    (appendChild, removeChild)

newtype ReleaseFunction = ReleaseFunction { runReleaser :: IO () }

instance Monoid ReleaseFunction where
  mempty = ReleaseFunction $ return ()
  (ReleaseFunction a) `mappend` (ReleaseFunction b) = ReleaseFunction (a >> b)

class (Typeable frame) => FrameClass frame where
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
  updateElement element text = do Element.setInnerText element (Just text)
                                  return (toElement element)

createChildElement :: (DocumentClass doc, NodeClass p, GlibString t) => doc -> p -> t -> IO Element
createChildElement doc parent tag = do
  element <- Doc.createElement doc (Just tag)
  _ <- Node.appendChild parent element
  return (M.fromJust element)

appendChildren :: (NodeClass parent, NodeClass child) => parent -> [child] -> IO ()
appendChildren parent = mapM_ (\x -> Node.appendChild parent (Just x))

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

-- Parser ------------------------------------------------------------------------------------------

type Elements     = [Element]
type RootElements = Elements

-- Трансформирует список HTML-токенов в WebKit элементы,
-- сохраняя при этом структуру. (изоморфизм)
-- NOTE: На некорректный список токенов выдает ошибку.
-- Также возвращает список root-элементов для последующего
-- их добавления в DOM и создания функции удаления. (ReleaseFunction)
tokensToNodes :: (DocumentClass doc) => doc -> [Token] -> IO (RootElements, Elements)
tokensToNodes doc tokens =
  let (Right tokensForest) = Tree.tokensToForest tokens
  in transformForest doc tokensForest
  where transformForest doc tokensForest = do
          elemsForest <- M.catMaybes <$> mapM (worker doc Nothing) tokensForest
          let roots = map rootLabel elemsForest
              nodes = (concat . map Tree.flatten) elemsForest
          return (roots, nodes)
          where worker _ (Just parent) (Node (ContentText text) []) = do
                  Element.setInnerText (castToHTMLElement parent) (Just text)
                  return Nothing
                worker doc mParent (Node tagToken forest) =
                  let f = case tagToken of TagOpen tag attrs -> handleTag tag attrs
                                           TagSelfClose tag attrs -> handleTag tag attrs
                  in f forest doc mParent

                handleTag tag attrs forest doc mParent = do
                  element <- toElement doc mParent tag attrs
                  children <- M.catMaybes <$> mapM (worker doc (Just element)) forest
                  appendChildren element (map rootLabel children)
                  return $ Just (Node element children)

                toElement doc mParent tag attrs = do
                  (Just element) <- case mParent of
                                      Just parent -> Just <$> createChildElement doc parent tag
                                      Nothing     -> Doc.createElement doc (Just tag)
                  setAttrs element attrs
                  return element
                  where setAttrs _ [] = return ()
                        setAttrs element ((Attr attrName attrValue):xs) = do
                          Element.setAttribute element attrName attrValue
                          setAttrs element xs

defaultFunRelease :: (NodeClass parent) => parent -> RootElements -> ReleaseFunction
defaultFunRelease parent roots = ReleaseFunction $ mapM_ (Node.removeChild parent . Just) roots

asChildrenOf :: (NodeClass parent) => parent -> IO (RootElements, Elements)
                                             -> IO (ReleaseFunction, Elements)
asChildrenOf parent f = f >>= \(roots, elements) -> do
  appendChildren parent roots
  return (defaultFunRelease parent roots, elements)
