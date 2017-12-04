{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Init
  ( initInterface ) where

import Control.Monad.IO.Class (liftIO)
import Data.Default                        (Default(..))
import Control.Monad.Trans.Reader          (ReaderT(..))
import Data.IORef                          (newIORef)
import Control.Monad                       (void)
import Control.Concurrent                  (forkIO)
import Control.Concurrent.MVar             (takeMVar, newEmptyMVar)
import System.Glib.Attributes              (AttrOp((:=)))
import Graphics.UI.Gtk                     (on, Window)
import Graphics.UI.Gtk.WebKit.WebView      (WebView)
import Graphics.UI.Gtk.WebKit.DOM.Document (Document)
--import Login.Logic                         (signInSetup)
import Text.HTML.Parser
import Inline.StyleSheet
import Server.General
import Types.General                       (App(..))
import Graphics.General
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement hiding (click)
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement
import qualified Network.Socket                      as Sock  (close)
import qualified System.Glib.Attributes              as Attrs (set)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document as Doc
import qualified Graphics.UI.Gtk                     as Gtk
import qualified Graphics.UI.Gtk.WebKit.WebView      as WV
import Graphics.UI.Gtk.WebKit.DOM.Element
import Graphics.UI.Gtk.WebKit.DOM.Document (createElement)
import Graphics.UI.Gtk.WebKit.DOM.Node
import Graphics.UI.Gtk.WebKit.DOM.EventM hiding (on)
import AdaptiveColumns
import Types.General
import Data.Matrix (fromList)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element             as WebKit (getStyle)
import qualified Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration as WebKit (setProperty)
import Graphics.Frames.Entry
import Graphics.Data.Frames
import Data.Set as S

import Graphics.UI.Gtk.WebKit.DOM.HTMLButtonElement (castToHTMLButtonElement, HTMLButtonElement)
import Data.Text
import Network.Socket (Socket)
import Data.Matrix                          as M    (Matrix, fromList)
import Graphics.UI.Gtk.WebKit.CacheModel
import Graphics.UI.Gtk.WebKit.WebSettings

default (Text)

data WinParameters = WinParameters
  { winGtk :: Window
  , winWV  :: WebView
  , winDoc :: Document }

initLogin :: String -> (WinParameters -> IO ()) -> IO ()
initLogin winTitle winBehavior = do
  wv  <- WV.webViewNew
  win <- Gtk.windowNew

  Attrs.set win [Gtk.containerChild := wv, Gtk.windowTitle := winTitle]
  Gtk.widgetSetSizeRequest win 600 650
  let loginTemplate = $(loadHtml "code.html")
  WV.webViewLoadString wv loginTemplate (Just ("text/html" :: String)) ""

  void $ wv `on` WV.documentLoadFinished $ \_ -> do
    _ <- Gtk.onDelete win $ \_ -> Gtk.windowIconify win >> return True
    _ <- Gtk.onDestroy win $ Gtk.mainQuit
    (Just doc) <- WV.webViewGetDomDocument wv
    winBehavior $ WinParameters win wv doc
    void $ wv `on` WV.loadFinished $ \_ -> Gtk.widgetShowAll win

onPressEnter :: (Doc.DocumentClass doc) => IO () -> AppM doc ()
onPressEnter action = void $ do
  win <- getAppM appWin
  liftIO (onPress win "Return" action)

initInterface :: IO ()
initInterface =
  let winTitle    = "Conv'Em" :: String
  in do
    _              <- forkIO initServer
    sock           <- initSocket ClientSocket
    mvarAuthorized <- newEmptyMVar
    setCacheModel CacheModelDocumentViewer
    _              <- Gtk.initGUI
    initLogin winTitle $ \winParams ->
      let doc = winDoc winParams
          win = winGtk winParams
          wv  = winWV  winParams
      in do
        -- FIXME: Just as example.
        appRef <- newIORef (App doc win wv sock mvarAuthorized def S.empty)
        flip runReaderT appRef . withLoadingSplash $ do
          withFrame def $ \(hdrFrame, releaser, wrapper) ->
            let frameElements = unwrapFrame hdrFrame
            in do liftIO $ do bindQuit win sock (btnQuit frameElements)
                              onClick (btnSettings frameElements) $
                                runReaderT (releaseFrame releaser wrapper) appRef
                  (Just elBtnAbout) <- askForElement (btnAbout . unwrapFrame)
                  void . liftIO $ onClick elBtnAbout $ runReaderT (translateInterface RUS) appRef
                  withFrame (def :: EntryFrameData) $ \(entryElements@(EntryFrameElements{..}), releaser, _) -> do
                    onPressEnter $ do
                      (Just phoneCode) <- getValue (castToHTMLInputElement elInpCode)
                      (Just phoneNum) <- getValue (castToHTMLInputElement elInpPhone)
                      flip runReaderT appRef $ do
                        liftIO (runReleaser releaser)
                        withFrame (def { ecPhone = append (append phoneCode " ") phoneNum }) $ \(_, _, _) -> return ()
                    liftIO (setupEntry (def :: EntryFrameData) entryElements)
        --
    Gtk.mainGUI
    where bindQuit win sock btnQuit = onClick btnQuit $ do
            Gtk.widgetDestroy win
            Sock.close sock
            Gtk.mainQuit
