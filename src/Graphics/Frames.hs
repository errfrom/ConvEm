{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Graphics.Frames
  ( withLoadingSplash, unwrapFrame ) where

import Graphics.Data.Frames
import Data.Text                                    (Text)
import Data.Foldable                                (toList)
import Data.Tree                                    (Tree(..))
import Control.Monad.IO.Class                       (liftIO)
import Control.Monad                                (void)
import Types.General
import Text.HTML.Parser                             (Token(..), Attr(..))
import Graphics.UI.Gtk                              (on)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement       (castToHTMLElement)
import Graphics.UI.Gtk.WebKit.DOM.Element           (Element)
import Graphics.UI.Gtk.WebKit.DOM.Document          (DocumentClass)
import qualified Data.Text                               as T       (pack)
import qualified Control.Concurrent                      as Conc    (forkIO)
import qualified Control.Concurrent.MVar                 as MVar    (putMVar, newEmptyMVar, takeMVar)
import qualified Graphics.UI.Gtk                         as Gtk     (postGUIAsync)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element      as Element (setAttribute)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document     as Doc     (createElement, getBody)
import qualified Graphics.UI.Gtk.WebKit.DOM.Node         as Node    (appendChild)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLElement  as Element (setInnerText)
import qualified Graphics.UI.Gtk.WebKit.WebView          as WV      (loadFinished)

default (Text)

-- Header ------------------------------------------------------------------------------------------

instance FrameClass HeaderFrameData where
  newtype FrameElements HeaderFrameData el = HeaderFrameElements
    { unwrapFrame :: (HeaderFrame el) }

  updateFrame _ (HeaderFrameElements elFrame) dataFrame =
    mapM_ (\(el, text) -> Element.setInnerText (castToHTMLElement el) (Just text)) $
      zip (toList elFrame) (toList dataFrame)

  initFrame doc frameHeader = do
    frame@HeaderFrame{..} <- traverse (initBtnLink doc) frameHeader
    (Just body) <- Doc.getBody doc
    (roots, elForest) <- tokensToNodes doc headerHtml
    let (Node _ [Node boxAction [], Node boxNavigation []]) = head elForest
    appendChildren boxAction     [btnQuit]
    appendChildren boxNavigation [btnSettings, btnAbout]
    appendChildren body roots
    return (defaultFunRelease body roots, HeaderFrameElements frame)
    where headerHtml = TagOpen "header" []
                     :   TagOpen "div" [Attr "id" "left-box"] : TagClose "div"
                     :   TagOpen "div" [Attr "id" "right-box"] : TagClose "div"
                     : TagClose "header" : []

          initBtnLink :: (DocumentClass doc) => doc -> Text -> IO Element
          initBtnLink doc text = do element <- worker doc
                                    Element.setInnerText (castToHTMLElement element) (Just text)
                                    return element
            where worker doc = do (Just btnLink) <- Doc.createElement doc (Just "button")
                                  Element.setAttribute btnLink (T.pack "class") "btn-link"
                                  return btnLink

-- Loading -----------------------------------------------------------------------------------------

withLoadingSplash :: (DocumentClass doc) => AppM doc a -> AppM doc a
withLoadingSplash action = do
  liftIO $ putStrLn "START"
  doc <- getAppM appDoc
  wv  <- getAppM appWV
  releaseFunction <- liftIO $ do (Just body) <- Doc.getBody doc
                                 ([loadingContainer], _) <- tokensToNodes doc loadingHtml
                                 void $ Node.appendChild body (Just loadingContainer)
                                 return $ defaultFunRelease body [loadingContainer]
  mvarLoadFinished <- liftIO $ do
    mvarLoadFinished' <- MVar.newEmptyMVar
    liftIO . void $ wv `on` WV.loadFinished $ \_ -> MVar.putMVar mvarLoadFinished' True
    return mvarLoadFinished'
  res <- action
  liftIO . void. Conc.forkIO $ do
    _ <- MVar.takeMVar mvarLoadFinished
    Gtk.postGUIAsync (runReleaser releaseFunction >> allowAnimations doc)
  return res
  where loadingHtml = TagOpen "div" [Attr "id" "splash-container"]
                    :   TagOpen "div" [Attr "id" "splash"]
                    :     TagOpen "div" [Attr "class" "sk-three-bounce"]
                    :        TagOpen "div" [Attr "class" "sk-bounce1"] : TagClose "div"
                    :        TagOpen "div" [Attr "class" "sk-bounce2"] : TagClose "div"
                    :        TagOpen "div" [] : TagClose "div"
                    :     TagClose "div"
                    :   TagClose "div"
                    : TagClose "div" : []

        allowAnimations doc = do (Just body) <- Doc.getBody doc
                                 Element.setAttribute body "class" "animation-allowed"


-- Entry -------------------------------------------------------------------------------------------

entryHtml :: EntryFrameData -> [Token]
entryHtml EntryFrame{..} =
    TagOpen "div" [Attr "id" "form"]
  :   TagOpen "div" [Attr "id" "header-text"] : ContentText txtHeader : TagClose "div"
  :   TagOpen "div" [Attr "id" "advice-box"] : tokenizeAdvices txtDesc ++ TagClose "div"
  :   TagOpen "div" [Attr "class" "select-wrapper"]
  :     TagSelfClose "input" [ Attr "class" "select"
                             , Attr "type" "checkbox"
                             , Attr "id" "country-choose" ]
  :     TagOpen "input" [ Attr "placeholder" inpSearch
                        , Attr "class" "btn"
                        , Attr "for" "country-choose"
                        , Attr "value" "Belarus" ] : TagClose "input"
  :     TagOpen "ul" []
  :     TagClose "ul"
  :     TagOpen "div" [Attr "class" "horizontal", Attr "id" "phone-number"]
  :       TagOpen "input" [Attr "value" "+375"] : TagClose "input"
  :       TagOpen "input" [Attr "placeholder" inpPhone] : TagClose "input"
  :     TagClose "div"
  :     TagOpen "div" [Attr "id" "error-box"] : tokenizeAdvices txtErrors ++ TagClose "div"
  :   TagClose "div"
  : TagClose "div" : []
  where tokenizeAdvices []     = []
        tokenizeAdvices (x:xs) = TagOpen "div" [Attr "class" "advice-text"]
                               : ContentText x : TagClose "div" : tokenizeAdvices xs

instance FrameClass EntryFrameData where
  data FrameElements EntryFrameData el = EntryFrameElements
    { elForm :: el, elHeader :: el, elAdviceBox :: el, elErrorBox :: el
    , elInpSearch :: el, elSelectCbox :: el, elSelectList :: el, elInpCode :: el
    , elInpPhone :: el }
  initFrame doc entryData@(EntryFrame{..}) = do
    (Just body) <- Doc.getBody doc
    (roots, elForest) <- tokensToNodes doc (entryHtml entryData)
    let (Node elForm [ Node elHeader [], Node elAdviceBox _
                     , Node _ [Node elSelectCbox [], Node elInpSearch [], Node elSelectList _]
                     , Node _ [Node elInpCode [], Node elInpPhone []]
                     , Node elErrorBox _ ]) = head elForest
    appendChildren body roots
    return (defaultFunRelease body roots, EntryFrameElements{..})
