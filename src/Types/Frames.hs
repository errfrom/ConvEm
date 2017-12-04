{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}

module Types.Frames
  ( Language(..), LocalizedClass(..)
  , AnyFrame(AnyFrame), ReleaseFunction(..), defaultFunRelease
  , FrameClass(..), FrameElements'
  , pattern (:>), appendChildren, tokensToNodes ) where

import GHC.Fingerprint.Type                   (Fingerprint)
import Data.Typeable                          (Typeable, typeOf, typeRepFingerprint)
import Data.Default                           (Default(..))
import System.Glib.UTFString                  (GlibString)
import Data.Text                              (Text)
import Data.Tree                              (Tree(..), Forest)
import Text.HTML.Parser                       (Token(..), Attr(..))
import Graphics.UI.Gtk.WebKit.DOM.Document    (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.Element     (Element)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement (castToHTMLElement)
import Graphics.UI.Gtk.WebKit.DOM.Node        (NodeClass)
import qualified Data.Maybe                             as M       (fromJust, catMaybes)
import qualified Text.HTML.Tree                         as Tree    (tokensToForest)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element     as Element (setAttribute)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLElement as Element (setInnerText)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document    as Doc     (createElement)
import qualified Graphics.UI.Gtk.WebKit.DOM.Node        as Node    (appendChild, removeChild)

default (Text)

-- Localization ------------------------------------------------------------------------------------

data Language = RUS | ENG

instance Default Language where def = ENG

class (Default t) => LocalizedClass t where
  -- NOTE: translate lang t | lang == def && t == def = t
  translate :: Language -> t -> t

-- Releasing ---------------------------------------------------------------------------------------

newtype ReleaseFunction = ReleaseFunction { runReleaser :: IO () }

defaultFunRelease :: (NodeClass parent) => parent -> RootElements -> ReleaseFunction
defaultFunRelease parent roots = ReleaseFunction $ mapM_ (Node.removeChild parent . Just) roots

-- Frame -------------------------------------------------------------------------------------------

data AnyFrame = forall frameData. FrameClass frameData =>
  AnyFrame (FrameElements frameData Element) frameData

-- These instances are needed for making Set.
typeRepFingerprint' :: (Typeable a) => a -> Fingerprint
typeRepFingerprint' = typeRepFingerprint . typeOf
instance Eq  AnyFrame where a == b = typeRepFingerprint' a == typeRepFingerprint' b
instance Ord AnyFrame where a <= b = typeRepFingerprint' a <= typeRepFingerprint' b

type FrameElements' frameData = FrameElements frameData Element

-- Frame is an area, which contains fixed amount of elements.
-- Frames could be managed without depending on each other.
class (Typeable frameData, LocalizedClass frameData) => FrameClass frameData where
  data FrameElements frameData :: * -> *
  updateFrame :: (DocumentClass doc) => doc -> FrameElements' frameData -> frameData -> IO ()
  initFrame   :: (DocumentClass doc) => doc
                                     -> frameData
                                     -> IO (ReleaseFunction, FrameElements' frameData)

-- Useful functions --------------------------------------------------------------------------------

createChildElement :: (DocumentClass doc, NodeClass p, GlibString t) => doc -> p -> t -> IO Element
createChildElement doc parent tag = do
  element <- Doc.createElement doc (Just tag)
  _ <- Node.appendChild parent element
  return (M.fromJust element)

appendChildren :: (NodeClass parent, NodeClass child) => parent -> [child] -> IO ()
appendChildren parent = mapM_ (\x -> Node.appendChild parent (Just x))

-- DOM -> WebKit -----------------------------------------------------------------------------------

pattern (:>) a b = Node a b

type RootElements = [Element]

-- Isomorphism. (Saves the structure and information)
-- Transforms the list of HTML-tokens to WebKit elements.
-- Also returns the list of root elements,
-- which play the role of connectors with the current DOM.
-- NOTE: Throws error on invalid DOM.
tokensToNodes :: (DocumentClass doc) => doc -> [Token] -> IO (RootElements, Forest Element)
tokensToNodes doc tokens =
  let (Right tokensForest) = Tree.tokensToForest tokens
  in transformForest doc tokensForest
  where transformForest doc tokensForest = do
          elemsForest <- M.catMaybes <$> mapM (worker doc Nothing) tokensForest
          let roots = map rootLabel elemsForest
          return (roots, elemsForest)
          where worker _ (Just parent) (Node (ContentText text) []) = do
                  Element.setInnerText (castToHTMLElement parent) (Just text)
                  return Nothing
                worker doc mParent (Node tagToken forest) =
                  let f = case tagToken of TagOpen tag attrs -> handleTag tag attrs
                                           TagSelfClose tag attrs -> handleTag tag attrs
                                           _  -> undefined
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
