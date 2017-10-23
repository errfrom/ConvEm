{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Init
  ( initInterface ) where

import Control.Monad.Trans.Reader          (ReaderT(..))
import Data.IORef                          (newIORef)
import Control.Monad                       (void)
import Control.Concurrent                  (forkIO)
import Control.Concurrent.MVar             (takeMVar, newEmptyMVar)
import System.Glib.Attributes              (AttrOp((:=)))
import Graphics.UI.Gtk                     (on, Window)
import Graphics.UI.Gtk.WebKit.WebView      (WebView)
import Graphics.UI.Gtk.WebKit.DOM.Document (Document)
import Login.Logic                         (signInSetup)
import Inline.StyleSheet
import Server.General
import Types.General                       (App(..))
import Graphics.General
import Graphics.Data.Selectors
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement
import qualified Network.Socket                      as Sock  (close)
import qualified System.Glib.Attributes              as Attrs (set)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document as Doc
import qualified Graphics.UI.Gtk                     as Gtk
import qualified Graphics.UI.Gtk.WebKit.WebView      as WV

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
  let loginTemplate = $(loadHtml "login-template.html")
  WV.webViewLoadString wv loginTemplate (Just "text/html") ""
  Gtk.widgetShowAll win

  void $ wv `on` WV.documentLoadFinished $ \_ -> do
    (Just doc) <- WV.webViewGetDomDocument wv
    winBehavior $ WinParameters win wv doc
    _ <- Gtk.onDelete win $ \_ -> Gtk.windowIconify win >> return True
    _ <- Gtk.onDestroy win $ Gtk.mainQuit
    return ()

initInterface :: IO ()
initInterface =
  let winTitle    = "Conv'Em" :: String
      selBtnQuit' = unSel selBtnQuit
  in do
    _    <- Gtk.initGUI
    _    <- forkIO initServer
    sock <- initSocket ClientSocket
    mvarAuthorized <- newEmptyMVar
    initLogin winTitle $ \winParams ->
      let doc = winDoc winParams
          win = winGtk winParams
      in do
        appRef <- newIORef (App doc win sock mvarAuthorized)
        btnQuit <- Doc.getElementById doc selBtnQuit'
        maybe (selNonexistent selBtnQuit')
              (\btn -> do setInnerText (castToHTMLElement btn) (Just "Выйти")
                          bindQuit win sock btn) btnQuit
        runReaderT signInSetup appRef
    Gtk.mainGUI
    putStrLn "END."
    where bindQuit win sock btnQuit = onClick btnQuit $ do
            Gtk.widgetDestroy win
            Sock.close sock
            Gtk.mainQuit
