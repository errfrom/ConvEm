module GUI.Main
  ( initInterface ) where

--------------------------------------------------------------------------------
-- Инициализирует графический интерфейс.
-- Описывает структуру взаимодействия пользователя с программой.
--------------------------------------------------------------------------------

import qualified Control.Concurrent              as Conc      (forkIO)
import           Control.Monad                                (void)
import qualified Control.Exception               as Exc       (SomeException(..))
import           Control.Exception                            (catch)
import qualified System.Exit                     as Exit      (ExitCode(..)
                                                              ,exitWith)
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

-- | Основная функция, запускающая
-- инициализацию и поддержку локального сервера
-- параллельно инициализации графического интерфейса GTK.
initInterface :: IO()
initInterface = do
  startLocalServer <- Conc.forkIO startLocalServer
  startGtk

-- | Запускает локальный сервер,
-- декорированный функцией setup
-- при помощи Threepenny-UI.
startLocalServer :: IO()
startLocalServer =
  let config = UICore.defaultConfig {UICore.jsPort = Just 8010}
  in UICore.startGUI config setup
  where setup window = void $
          return window # UICore.set UICore.title "DDChat"

-- | Инициализирует GTK GUI,
-- выступающий в роли браузера для описанного
-- функцией 'setup' интерфейса взаимодействия.
startGtk :: IO()
startGtk =
  let gtkInitErrorMsg = "Ошибка инициализации графического интерфейса."
      url             = "http://127.0.0.1:8010"
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
                     , Window.windowDefaultHeight := 400]
    Attrs.set scrolledWindow [ Container.containerChild := webView ]
    WebView.webViewLoadUri webView url
    Widget.onDestroy window Gtk.mainQuit
    Widget.widgetShowAll window
    Gtk.mainGUI
