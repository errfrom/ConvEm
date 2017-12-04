{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Graphics.Frames.Entry where

import Graphics.Data.Frames
import Data.Text                                    (Text)
import Data.Foldable                                (toList)
import Data.List                                    (isPrefixOf)
import Data.Tree                                    (Tree(..))
import Data.Char                                    (isDigit)
import Control.Monad.IO.Class                       (liftIO)
import Control.Monad                                (void)
import Types.General
import Text.HTML.Parser                             (Token(..), Attr(..))
import Graphics.UI.Gtk                              (on)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement       (castToHTMLElement)
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement  (castToHTMLInputElement)
import Graphics.UI.Gtk.WebKit.DOM.Element           (Element, castToElement)
import Graphics.UI.Gtk.WebKit.DOM.Document          (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.EventM     hiding (on)
import Graphics.General                             (onClick)
import qualified Data.Maybe                              as M       (catMaybes)
import qualified Data.Text                               as T       (pack, isPrefixOf, drop, unpack)
import qualified Control.Concurrent                      as Conc    (forkIO)
import qualified Data.IORef                              as IORef   (newIORef, readIORef, writeIORef)
import qualified Control.Concurrent.MVar                 as MVar    (putMVar, newEmptyMVar, takeMVar)
import qualified Graphics.UI.Gtk                         as Gtk     (postGUIAsync)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element      as Element (setAttribute, getAttribute, setClassName, focus
                                                                    ,getTagName, input, click)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as Inp (setValue, getValue, setChecked)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document     as Doc     (createElement, getBody)
import qualified Graphics.UI.Gtk.WebKit.DOM.Node         as Node    (appendChild, getParentNode)
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
  :   TagOpen "div" [Attr "class" "header-text"] : ContentText txtHeader : TagClose "div"
  :   TagOpen "div" [Attr "id" "advice-box"] : tokenizeAdvices txtDesc ++ TagClose "div"
  :   TagOpen "div" [Attr "class" "select-wrapper"]
  :     TagSelfClose "input" [ Attr "class" "select"
                             , Attr "type" "checkbox"
                             , Attr "id" "country-choose" ]
  :     TagOpen "input" [ Attr "placeholder" inpSearch
                        , Attr "class" "btn"
                        , Attr "for" "country-choose"
                        , Attr "value" defOption ] : TagClose "input"
  :     TagOpen "ul" [] : tokenizeOptions cOptions ++ TagClose "ul"
  :   TagClose "div"
  :   TagOpen "div" [Attr "class" "horizontal", Attr "id" "phone-number"]
  :     TagOpen "input" [Attr "value" "+"] : TagClose "input"
  :     TagOpen "input" [Attr "id" "inp-mask"] : TagClose "input"
  :     TagOpen "input" [Attr "id" "inp-phone-number", Attr "placeholder" inpPhone] : TagClose "input"
  :   TagClose "div"
  :   TagOpen "div" [Attr "id" "error-box"] : tokenizeAdvices txtErrors ++ TagClose "div"
  : TagClose "div" : []
  where tokenizeAdvices []     = []
        tokenizeAdvices (x:xs) = TagOpen "div" [Attr "class" "advice-text"]
                               : ContentText x : TagClose "div" : tokenizeAdvices xs

        tokenizeOptions [] = []
        tokenizeOptions ((CountryOption code country _):xs) =
            TagOpen "li" []
          :   ContentText (T.pack country)
          :   TagOpen "span" [Attr "class" "phone-prefix"]
          :     ContentText code
          :   TagClose "span"
          : TagClose "li" : tokenizeOptions xs

instance FrameClass EntryFrameData where
  data FrameElements EntryFrameData el = EntryFrameElements
    { elForm :: el, elHeader :: el, elAdviceBox :: el, elErrorBox :: el
    , elInpSearch :: el, elSelectCbox :: el, elSelectList :: el, elInpCode :: el, elInpMask :: el
    , elInpPhone :: el, countries :: [(CountryOption, el)] }
  initFrame doc entryData@(EntryFrame{..}) = do
    (Just body) <- Doc.getBody doc
    (roots, elForest) <- tokensToNodes doc (entryHtml entryData)
    let (elForm :> [ elHeader :> _, elAdviceBox :> _
                   , _ :> [ elSelectCbox :> _, elInpSearch :> _, elSelectList :> optionEls]
                   , _ :> [ elInpCode :> _, elInpMask :> _, elInpPhone :> _]
                   , elErrorBox :> _ ]) = head elForest
    let a = map (\(Node el _) -> el) optionEls
        countries = zip cOptions a
    appendChildren body roots
    return (defaultFunRelease body roots, EntryFrameElements{..})

instance FrameClass EntryCodeFrame where
  data FrameElements EntryCodeFrame el = EntryCodeFrameElements
         { elCodeHeader :: el, elPhoneHeader :: el, elCodeSection :: el }
  initFrame doc entryData = do
    (Just body) <- Doc.getBody doc
    (roots, elForest) <- tokensToNodes doc (entryCodeHtml entryData)
    let (_ :> [ elCodeHeader :> _, elPhoneHeader :> _, elCodeSection :> _ ]) = head elForest
    appendChildren body roots
    return (defaultFunRelease body roots, EntryCodeFrameElements{..})
    where entryCodeHtml EntryCodeFrame{..} =
              TagOpen "div" [Attr "id" "form"]
            :   TagOpen "div" [Attr "class" "header-text", Attr "id" "code-header-text"]
            :     ContentText ecHeader
            :   TagClose "div"
            :   TagOpen "div" [Attr "class" "header-text", Attr "id" "num-header-text"]
            :     ContentText ecPhone
            :   TagClose "div"
            :   TagOpen "section" []
            :     TagOpen "input" [Attr "class" "one-char-input", Attr "maxlength" "1"] : TagClose "input"
            :     TagOpen "input" [Attr "class" "one-char-input", Attr "maxlength" "1"] : TagClose "input"
            :     TagOpen "input" [Attr "class" "one-char-input", Attr "maxlength" "1"] : TagClose "input"
            :     TagOpen "input" [Attr "class" "one-char-input", Attr "maxlength" "1"] : TagClose "input"
            :   TagClose "section"
            : TagClose "div" : []

-- TODO : Refactor it and set up the search.
setupEntry :: EntryFrameData -> FrameElements' EntryFrameData -> IO ()
setupEntry EntryFrame{..} EntryFrameElements{..} = do
  maskContainer <- IORef.newIORef ""
  setupSelect maskContainer
  setupInpCode maskContainer
  setupInpPhoneBehavior maskContainer

  where bindEvent el event action     = newListener action >>= flip (addListener el event) True
        renderMask (PhoneMask [])     = ""
        renderMask (PhoneMask (x:xs)) = ([1..x] *> "*") ++ " " ++ renderMask (PhoneMask xs)
        inpSetValue el val            = Inp.setValue (castToHTMLInputElement el) (Just val)
        inpGetValue el                = Inp.getValue (castToHTMLInputElement el) >>= \(Just x) -> return x

---------------------------------------------------------------------------------------------------

        setupSelect maskContainer = do
          bindEvent elSelectCbox Element.click $ do
            -- By clicking on the select checkbox an options list appears.
            Element.setClassName elForm "only-search"
            inpSetValue elInpSearch ""
            Element.focus elInpSearch
          bindEvent elSelectList Element.click $ do
            -- There are options in list.
            -- So if user choose the list option he also click on some area of the list.
            -- According to the option has been chosen by the user, we change other elements.
            elChosen <- safeMouseToElement
            let ((CountryOption cCode cName mask), _) = getMatchedCountry elChosen
            Element.setClassName elForm ""
            inpSetValue elInpSearch cName
            Inp.setChecked (castToHTMLInputElement elSelectCbox) False
            let renderedMask = renderMask mask
            inpSetValue elInpMask renderedMask
            liftIO (IORef.writeIORef maskContainer renderedMask)
            inpSetValue elInpPhone ""
            inpSetValue elInpCode cCode
            Element.focus elInpPhone
          where safeMouseToElement = do
                  (Just node) <- mouseToElement
                  let element' = castToElement node
                  (Just nodeTagName) <- Element.getTagName element'
                  if nodeTagName == "SPAN"
                    then Node.getParentNode node >>= \(Just x) -> return (castToElement x)
                    else return element'
                getMatchedCountry elChosen = head . flip filter countries $ \(_, el) -> el == elChosen

---------------------------------------------------------------------------------------------------

        setupInpCode maskContainer =
          let cCodes = map (\(CountryOption{..}, _) -> cCode) countries
          in do
            val <- inpGetValue elInpCode
            cCodePrev <- IORef.newIORef val
            bindEvent elInpCode Element.input $ do
              --User is changing the country code.
              val <- inpGetValue elInpCode
              prevVal <- liftIO (IORef.readIORef cCodePrev)
              valChecked <- fmap T.pack (checkForPlus val)
              if prevVal == valChecked
                then return () -- Value is the same so why should I care?
                else do if any (T.isPrefixOf valChecked) cCodes
                          then do
                            liftIO (IORef.writeIORef cCodePrev valChecked)
                            -- This new value could be not only the prefix but the code.
                            case (filter (\(CountryOption{..}, _) -> cCode == valChecked) countries) of
                              [] -> do inpSetValue elInpSearch defOption
                                       inpSetValue elInpMask ""
                                       liftIO (IORef.writeIORef maskContainer "")
                                       Element.setAttribute elInpPhone "placeholder" inpPhone
                              [(CountryOption{..}, _)] -> do
                                inpSetValue elInpSearch cName
                                let maskRendered = renderMask cpMask
                                inpSetValue elInpMask maskRendered
                                liftIO (IORef.writeIORef maskContainer maskRendered)
                                Element.setAttribute elInpPhone "placeholder" ""
                                inpSetValue elInpPhone ""
                          else inpSetValue elInpCode prevVal -- Deny changing.
            where checkForPlus val
                   | "+" `isPrefixOf` val = return val
                   | otherwise            = let valChecked = "+" ++ val
                                            in do inpSetValue elInpCode valChecked
                                                  return valChecked

---------------------------------------------------------------------------------------------------

        setupInpPhoneBehavior maskContainer = do
           val <- inpGetValue elInpPhone
           phonePrev <- IORef.newIORef val
           -- Changing of the value is accompanied by comparing it with the mask.
           bindEvent elInpPhone Element.input $ do
             val <- inpGetValue elInpPhone
             mask <- liftIO $ IORef.readIORef maskContainer
             prevVal <- liftIO (IORef.readIORef phonePrev)
             if prevVal == val
               then return ()
               else if (not $ validateValue val mask)
                      then inpSetValue elInpPhone prevVal
                      -- We just adapt our new value to the mask.
                      else if mask == ""
                             then inpSetValue elInpPhone val >> liftIO (IORef.writeIORef phonePrev val)
                             else
                               let strippedValue = filter isDigit val
                                   newValue      = worker strippedValue mask
                                   newMask       = ([1..length newValue] *> " ") ++ drop (length newValue) mask
                               in do inpSetValue elInpPhone newValue
                                     inpSetValue elInpMask  newMask
                                     liftIO (IORef.writeIORef phonePrev newValue)

          where validateValue val mask
                  | not . all (\x -> isDigit x || x == ' ') $ val               = False
                  | mask == ""                                                  = True
                  | length (filter isDigit val) > length (filter (/= ' ') mask) = False
                  | otherwise                                                   = True

                worker strippedValue@(x:xs) (y:ys)
                  | y /= ' '  = x   : worker xs ys
                  | otherwise = ' ' : worker strippedValue ys
                worker _ []  = []
                worker [] ys = []

                worker [] ys = []
