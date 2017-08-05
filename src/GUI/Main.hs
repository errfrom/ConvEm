module GUI.Main
  ( initInterface ) where

--------------------------------------------------------------------------------
-- Инициализирует графический интерфейс.
-- Описывает структуру взаимодействия пользователя с программой.
--------------------------------------------------------------------------------

import qualified Control.Concurrent              as Conc      (forkIO
                                                              ,killThread)
import           Control.Monad                                (void)
import qualified Control.Exception               as Exc       (SomeException(..))
import           Control.Exception                            (catch)
import qualified System.Exit                     as Exit      (ExitCode(..)
                                                              ,exitWith)
import qualified System.Process                  as Process   (system
                                                              ,readProcessWithExitCode)
import qualified System.Info                     as SysInfo   (os)
import qualified Graphics.UI.Threepenny.Core     as UICore
import           Graphics.UI.Threepenny.Core                  ((#), (#+))
import qualified Graphics.UI.Gtk.General.General as Gtk       (initGUI, mainQuit
                                                              ,mainGUI)
import qualified Graphics.UI.Gtk.Windows.Window  as Window    (windowNew
                                                              ,windowTitle
                                                              ,windowDefaultWidth
                                                              ,windowDefaultHeight)
import qualified Graphics.UI.Gtk.WebKit.WebView  as WebView   (webViewNew
                                                              ,webViewLoadUri)
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow
                                                 as SWindow   (scrolledWindowNew)
import qualified Graphics.UI.Gtk.Abstract.Widget as Widget    (onDestroy
                                                              ,widgetShowAll)
import qualified Graphics.UI.Gtk.Abstract.Container
                                                 as Container (containerChild)
import qualified System.Glib.Attributes          as Attrs     (set)
import           System.Glib.Attributes                       (AttrOp((:=)))

import qualified GUI.Login                       as Login     (loginForm)

import qualified Graphics.UI.Threepenny.Elements as Elems

-- | Основная функция, запускающая
-- инициализацию и поддержку локального сервера
-- параллельно инициализации графического интерфейса GTK.
initInterface :: IO()
initInterface =
  let portId = 8010
  in do
  _ <- Conc.forkIO (startLocalServer portId)
  startGtk portId

-- | Запускает локальный сервер,
-- декорированный функцией setup
-- при помощи Threepenny-UI.
startLocalServer :: Int -> IO()
startLocalServer portId =
  let config = UICore.defaultConfig {UICore.jsPort = Just portId}
  in UICore.startGUI config setup
  where setup window = void $ do
          return window # UICore.set UICore.title "DDChat"
          loginForm <- Login.loginForm
          UICore.getBody window #+ [UICore.element loginForm]

-- | Инициализирует GTK GUI,
-- выступающий в роли браузера для описанного
-- функцией 'setup' интерфейса взаимодействия.
startGtk :: Int -> IO()
startGtk portId =
  let gtkInitErrorMsg = "Ошибка инициализации графического интерфейса."
      url             = "http://127.0.0.1:" ++ (show portId)
  in do
    Gtk.initGUI `catch` (\(Exc.SomeException _) -> do
      putStrLn gtkInitErrorMsg
      Exit.exitWith (Exit.ExitFailure 1))
    window         <- Window.windowNew
    scrolledWindow <- SWindow.scrolledWindowNew Nothing Nothing
    webView        <- WebView.webViewNew
    Attrs.set window [ Container.containerChild   := scrolledWindow
                     , Window.windowTitle         := "DDChat"
                     , Window.windowDefaultWidth  := 500
                     , Window.windowDefaultHeight := 400 ]
    Attrs.set scrolledWindow [ Container.containerChild := webView ]
    WebView.webViewLoadUri webView url
    Widget.onDestroy window (safeQuit portId)
    Widget.widgetShowAll window
    Gtk.mainGUI

-- | Убивает процессы, использующие
-- порт сервера.
-- NOTE: взаимодействует с Shell на Linux.
-- TODO: поддержка Windows и MacOS.
safeQuit :: Int -> IO()
safeQuit portId = do
  -- Определение ID процесса системным способом
  -- по переданному номеру порта.
  if (SysInfo.os == "linux")
    then getPidByPortId portId
    else return ()
  Gtk.mainQuit
  where linuxReadProcess portId =
          let portArg = ":" ++ (show portId)
          in Process.readProcessWithExitCode "lsof" ["-t", "-i", portArg] ""

        linuxKillProcess readProcessOutput =
          let command = "kill -9 " ++ readProcessOutput
          in Process.system command

        getPidByPortId portId = do
          (exitCode, stdout', _) <- linuxReadProcess portId
          case exitCode of
            Exit.ExitFailure _ -> return ()
            Exit.ExitSuccess   -> (void . linuxKillProcess) stdout'
