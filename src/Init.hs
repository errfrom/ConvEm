{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Init
  ( initInterface ) where

import Control.Exception                           (catch)
import Control.Monad                               (void)
import Control.Monad.Reader                        (runReader)
import Graphics.UI.Threepenny.Core      hiding     (Window)
import Graphics.UI.Gtk                  hiding     (set, disconnect, on)
import Graphics.UI.Gtk.WebKit.WebView
import Templates.Inline                            (Selector(..), inlineCSS
                                                   ,inlineHTML, inlineJS)
import Server.General
import System.Glib.Attributes                      (AttrOp((:=)))
import qualified System.Glib.Signals    as Sig     (on)
import qualified System.Glib.Attributes as Attrs   (set)
import qualified System.Exit            as Exit    (ExitCode(..), exitWith)
import qualified Control.Concurrent     as Conc    (forkIO, threadDelay)
import qualified Control.Exception      as Exc     (SomeException(..))
import qualified System.Info            as SysInfo (os)
import qualified System.Process         as Process (system
                                                   ,readProcessWithExitCode)

import Login.Logic.Auth (authForm)

type PortId = Int

-- Запускает локальный UI сервер и GTK GUI обработчик.
initInterface :: IO()
initInterface =
  let portId = 50051
  in do
    _ <- Conc.forkIO (initServer)
    startUIServer portId
    initGtk portId
    mainGUI

startUIServer :: PortId -> IO ()
startUIServer portId = do
  let config = defaultConfig { jsPort                     = Just portId
                             , jsWindowReloadOnDisconnect = False }
  void $ Conc.forkIO (startGUI config setup)
  where setup window  = do -- TODO: разбить на функции
          _ <- return window
          on disconnect window $ \_ -> (liftIO . Conc.forkIO) $ do
            Conc.threadDelay 1000000
            liftIO (safeQuit portId)
          mainStyle  <- mkElement "style" # set (attr "id") "main-style"
          fontsStyle <- mkElement "style" # set (attr "id") "fonts-style"
          _ <- getHead window #+ [ element mainStyle, element fontsStyle ]
          $(inlineCSS  "./static/css/login.css"   SelId  "main-style")
          $(inlineCSS  "./static/css/fonts.css"   SelId  "fonts-style")
          $(inlineHTML "./static/html/auth.html"  SelTag "body")
          sock <- liftIO (initSocket ClientSocket)
          runReader authForm sock

initGtk :: PortId -> IO ()
initGtk portId =
  let url      = "http://127.0.0.1:" ++ (show portId)
      winTitle = "Glob"
  in do
    _       <- safeInitGUI -- TODO
    window  <- windowNew
    webView <- webViewNew
    setSizeSettings window
    Attrs.set window [containerChild := webView, windowTitle := winTitle]
    setErrHandler portId webView
    webViewLoadUri webView url
    --_ <- onDelete window $ \_ -> windowIconify window >> return True
    _ <- onDestroy window (safeQuit portId)
    widgetShowAll window

  where setSizeSettings window =
          let minSize = (615, 520) :: (Int, Int)
              defSize = 700
          in do
            Attrs.set window [ windowDefaultWidth  := defSize
                             , windowDefaultHeight := defSize ]
            widgetSetSizeRequest window (fst minSize) (snd minSize)

        safeInitGUI =
          let gtkInitErrorMsg = "Ошибка инициализации графического интерфейса."
          in do
            initGUI `catch` \(Exc.SomeException _) -> do
              putStrLn gtkInitErrorMsg
              Exit.exitWith (Exit.ExitFailure 1)

        setErrHandler portId webView = void $
          Sig.on webView loadError $ \_ (_ :: String) _ -> do
            safeQuit portId
            return True

-- | Убивает процессы, использующие
-- порт сервера.
-- NOTE: взаимодействует с Shell на Linux.
-- TODO: поддержка Windows и MacOS.
safeQuit :: PortId -> IO()
safeQuit portId = do
  -- Определение ID процесса системным способом
  -- по переданному номеру порта.
  if (SysInfo.os == "linux")
    then getPidByPortId portId
    else return ()
  mainQuit
  where linuxReadProcess portId' =
          let portArg = ":" ++ (show portId')
          in Process.readProcessWithExitCode "lsof" ["-t", "-i", portArg] ""

        linuxKillProcess readProcessOutput =
          let command = "kill -9 " ++ readProcessOutput
          in Process.system command

        getPidByPortId portId' = do
          (exitCode, stdout', _) <- linuxReadProcess portId'
          case exitCode of
            Exit.ExitFailure _ -> return ()
            Exit.ExitSuccess   -> (void . linuxKillProcess) stdout'
