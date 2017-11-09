{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE RecordWildCards    #-}

module Graphics.Frames where

import Data.Text                                    (Text)
import Control.Monad                                (void)
import Types.General
import Data.Typeable                                (Typeable)
import Graphics.Data.Selectors
import Graphics.UI.Gtk.WebKit.DOM.HTMLButtonElement (castToHTMLButtonElement)
import Graphics.UI.Gtk.WebKit.DOM.Element           (Element)
import Graphics.UI.Gtk.WebKit.DOM.Document          (DocumentClass)
import qualified Data.Text                           as T       (pack)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element  as Element (setAttribute)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document as Doc     (createElement, getBody)
import qualified Graphics.UI.Gtk.WebKit.DOM.Node     as Node    (removeChild)

data HeaderFrame a = HeaderFrame
  { btnQuit      :: a
  , btnSettings  :: a
  , btnAbout     :: a
  , btnSwitchFst :: a
  , btnSwitchSnd :: a }
  deriving (Typeable, Functor, Foldable, Traversable)

instance FrameClass HeaderFrame where
  initFrame doc frameHeader = do
    frame@HeaderFrame{..} <- traverse (initBtnLink doc) frameHeader
    (Just body) <- Doc.getBody doc
    boxAction     <- createContainer doc body selBoxAction    [btnQuit, btnSettings, btnAbout]
    boxNavigation <- createContainer doc body selBoxNavigation [btnSwitchFst, btnSwitchSnd]
    header        <- createChildElement doc body "header"
    appendChildren header [boxAction, boxNavigation]
    let funRelease = ReleaseFunction . void . Node.removeChild body $ (Just header)
    return (funRelease, frame)
    where initBtnLink :: (DocumentClass doc) => doc -> Text -> IO Element
          initBtnLink doc text = worker doc >>= flip updateElement text . castToHTMLButtonElement
            where worker doc = do (Just btnLink) <- Doc.createElement doc (Just "button")
                                  Element.setAttribute btnLink (T.pack "class") (unSel selBtnLink)
                                  return btnLink
