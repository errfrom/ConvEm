module Init
  ( initInterface ) where

--------------------------------------------------------------------------------
-- Инициализирует графический интерфейс.
-- Описывает структуру взаимодействия пользователя с программой.
--------------------------------------------------------------------------------

--Control-----------------------------------------------------------------------
import qualified Control.Concurrent as Conc (forkIO)
import Control.Monad                        (void)
import qualified Control.Exception  as Exc  (SomeException(..))
import Control.Exception                    (catch)
--System------------------------------------------------------------------------
import qualified System.Exit            as Exit    (ExitCode(..), exitWith)
import qualified System.Process         as Process (system
                                                   ,readProcessWithExitCode)
import qualified System.Info            as SysInfo (os)
import qualified System.Glib.Attributes as Attrs   (set)
import           System.Glib.Attributes            (AttrOp((:=)))
import qualified System.Directory       as Dir     (getCurrentDirectory
                                                   ,removeDirectoryRecursive)
--Threpenny---------------------------------------------------------------------
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as Elems (addStyleSheet)
import           Graphics.UI.Threepenny.Core              ((#))
--GTK---------------------------------------------------------------------------
import qualified Graphics.UI.Gtk.General.General          as Gtk
  (initGUI, mainQuit, mainGUI)
import qualified Graphics.UI.Gtk.Windows.Window           as Win
  (Window, windowNew, windowTitle, windowDefaultWidth
  ,windowDefaultHeight)
import qualified Graphics.UI.Gtk.Scrolling.ScrolledWindow as SWin
  (scrolledWindowNew)
import qualified Graphics.UI.Gtk.Abstract.Widget          as Widget
  (onDestroy, widgetShowAll
  ,widgetSetSizeRequest)
import qualified Graphics.UI.Gtk.Abstract.Container       as Container
  (containerChild)
--WebKit------------------------------------------------------------------------
import qualified Graphics.UI.Gtk.WebKit.WebView            as WV
  (webViewNew, webViewLoadUri)
--My----------------------------------------------------------------------------
import qualified Server.General as Server  (initServer)
import Main.Login.Manager       as Manager (initForms)
--------------------------------------------------------------------------------

-- | Основная функция, запускающая
-- инициализацию и поддержку локального сервера
-- параллельно инициализации графического интерфейса GTK.
initInterface :: IO()
initInterface =
  let portId = 8010
  in do
    mapM_ Conc.forkIO [ startLocalServer portId, Server.initServer ]
    _ <- initGtk portId
    Gtk.mainGUI

-- | Запускает локальный сервер,
-- декорированный функцией setup
-- при помощи Threepenny-UI.
startLocalServer :: Int -> IO()
startLocalServer portId= do
  currentDir <- Dir.getCurrentDirectory
  let pathStatic = currentDir ++ ("/static/")
      config = UI.defaultConfig { UI.jsPort   = Just portId
                                , UI.jsStatic = Just pathStatic }
  UI.startGUI config setup
  where setup window  = void $ do
          _ <- return window # UI.set UI.title "DDChat"
          Elems.addStyleSheet window "fonts.css"
          Elems.addStyleSheet window "login.css"
          Manager.initForms

-- | Инициализирует GTK GUI,
-- выступающий в роли браузера для описанного
-- функцией 'setup' интерфейса взаимодействия.
initGtk :: Int -> IO Win.Window
initGtk portId =
  let url         = "http://127.0.0.1:" ++ (show portId)
      minSize     = (400, 555) :: (Int, Int)
      defaultSize = 700
  in do
    _ <- improvedInitGUI
    window         <- Win.windowNew
    scrolledWindow <- SWin.scrolledWindowNew Nothing Nothing
    webView        <- WV.webViewNew
    Attrs.set window [ Container.containerChild := scrolledWindow
                     , Win.windowTitle          := "Sheer"
                     , Win.windowDefaultWidth   := defaultSize
                     , Win.windowDefaultHeight  := defaultSize ]
    -- Устанавливаем минимальные значения размеров окна
    Widget.widgetSetSizeRequest window (fst minSize) (snd minSize)
    Attrs.set scrolledWindow [ Container.containerChild := webView ]
    WV.webViewLoadUri webView url
    _ <- Widget.onDestroy window (safeQuit portId)
    Widget.widgetShowAll window
    return window
  where improvedInitGUI =
          let gtkInitErrorMsg = "Ошибка инициализации графического интерфейса."
          in do
            Gtk.initGUI `catch`
              \(Exc.SomeException _) -> do
                putStrLn gtkInitErrorMsg
                Exit.exitWith (Exit.ExitFailure 1)

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
            Exit.ExitSuccess   -> do
              current <- Dir.getCurrentDirectory
              Dir.removeDirectoryRecursive (current ++ "/.static") `catch`
                \(Exc.SomeException _) -> return()
              (void . linuxKillProcess) stdout'
