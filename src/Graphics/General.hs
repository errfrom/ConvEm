{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Graphics.General where

import Data.Text                                    (Text, unpack)
import Control.Concurrent.MVar                      (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class                       (liftIO)
import Control.Monad                                (void)
import Control.Monad.Trans.Reader                   (ReaderT(..), ask)
import Control.Exception                            (IOException, catch)
import Graphics.UI.Gtk.WebKit.DOM.EventTarget       (EventTargetClass)
import Graphics.UI.Gtk.WebKit.DOM.MouseEvent        (MouseEvent)
import Graphics.UI.Gtk.WebKit.DOM.Element           (Element)
import Graphics.UI.Gtk.WebKit.DOM.Document          (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement  (HTMLInputElement, castToHTMLInputElement)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement       (castToHTMLElement)
import Graphics.UI.Gtk.WebKit.DOM.HTMLButtonElement (castToHTMLButtonElement)
import Graphics.UI.Gtk.Abstract.Widget              (WidgetClass)
import Graphics.UI.Gtk.Gdk.Events                   (Event(Key))
import System.Glib.Signals                          (ConnectId)
import Types.ServerAction
import qualified Data.Maybe                                     as M       (catMaybes)
import qualified Inline.StyleSheet                              as Inline  (readHtml, appendHtml)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement    as Inp     (getValue)
import qualified Graphics.UI.Gtk.WebKit.DOM.NodeList            as NL      (getLength, item)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document            as Doc     (getElementById
                                                                           ,getElementsByTagName)
import qualified Graphics.UI.Gtk.WebKit.DOM.Element             as Element (setClassName)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLElement         as Element (setInnerText)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLButtonElement   as Button  (setDisabled)
import qualified Graphics.UI.Gtk.WebKit.DOM.EventTarget         as Event   (addEventListener)
import qualified Graphics.UI.Gtk.WebKit.DOM.EventTargetClosures as Event   (eventListenerNew)
import qualified Graphics.UI.Gtk                                as Gtk     (onKeyRelease
                                                                           ,timeoutAdd
                                                                           ,postGUIAsync)

onMouseEvent :: (EventTargetClass self) => String -> self -> IO () -> IO ()
onMouseEvent eventName target action = do
  eventListener <- Event.eventListenerNew $ \(_ :: MouseEvent) -> action
  Event.addEventListener target eventName (Just eventListener) True

onClick, onFocus :: (EventTargetClass self) => self -> IO () -> IO ()

onClick = onMouseEvent "click"
onFocus = onMouseEvent "focus"

-- Устанавливает событие, происходящие при нажатии на конкретную клавишу.
onPress :: (WidgetClass self) => self -> Text -> IO () -> IO (ConnectId self)
onPress widget eventKeyName' action = --on widget keyReleaseEvent $ \


  let event (Key _ _ _ _ _ _ _ _ eventKeyName _)
       |eventKeyName == eventKeyName' = action >> return True
       |otherwise                     = return False
      event _                         = return False
  in Gtk.onKeyRelease widget event

{-
getValue :: (DocumentClass doc) => doc -> CSSSel -> IO (Maybe String)
getValue doc selId =
  let selId' = unSel selId
  in do
    inp <- Doc.getElementById doc selId'
    maybe (selNonexistent selId' >> return Nothing)
          (Inp.getValue . castToHTMLInputElement) inp -}

-- Возвращает все поля ввода, находящиеся в DOM.
getInputs :: (DocumentClass doc) => doc -> IO [HTMLInputElement]
getInputs doc = do
  (Just inputsNodeList) <- Doc.getElementsByTagName doc ("input" :: String)
  numInputs             <- NL.getLength inputsNodeList
  sequence [ NL.item inputsNodeList i | i <- [0..numInputs]] >>=
    return . map castToHTMLInputElement . M.catMaybes

{-
initNoConnBox :: (DocumentClass doc) => doc -> IO Element
initNoConnBox doc = do
  Inline.appendHtml doc "no-conn-box.html" $(Inline.readHtml "no-conn-box.html")
  (Just noConnBox) <- Doc.getElementById doc (unSel selNoConnBox)
  operateElemById doc selConnErrMsg $ flip setInnerText (dataConnErrMessage connErrDialogData)
  operateElemById doc selBtnRetry   $ flip setInnerText (dataBtnRetryActive connErrDialogData)
  return noConnBox

-- Выполняет действие, зависимое от состояния соединения с сетью.
-- В случае отсутствия соединения показывает соответствующее окно,
-- которое закроется только в случае успешного повторного выполнения действия.
withNoConnHandling :: (DocumentClass doc, ServerAction d) => doc -> IO (ServerActionResult d)
                                                                 -> IO (ServerActionResult d)
withNoConnHandling doc action = tryRunAction action $ do
  mvarActionResult <- newEmptyMVar
  Gtk.postGUIAsync $ do
    noConnBox <- getNoConnBox doc
    handleNoConn doc action mvarActionResult noConnBox
  takeMVar mvarActionResult
  where tryRunAction action handler = catch action $ \(_ :: IOException) -> handler

        getNoConnBox doc = do
          -- Делает попытку найти noConnBox в DOM.
          -- При неудаче создает новый элемент noConnBox и встраивает в DOM.
          noConnBox <- Doc.getElementById doc (unSel selNoConnBox)
          maybe (initNoConnBox doc) (return . id) noConnBox

        setExecutingState htmlBtnRetry = do
          Button.setDisabled htmlBtnRetry True
          Element.setInnerText htmlBtnRetry (Just $ dataBtnRetryExecuting connErrDialogData)

        setStartingState htmlBtnRetry = do
          Button.setDisabled htmlBtnRetry False
          Element.setInnerText htmlBtnRetry (Just $ dataBtnRetryActive connErrDialogData)

        handleNoConn doc action mvarActionResult noConnBox = do
          Element.setClassName noConnBox (unSel selShown)
          operateElemById doc selBtnRetry $ \btnRetry -> onClick btnRetry $
            let htmlBtnRetry = castToHTMLButtonElement btnRetry
            in void $ do setExecutingState htmlBtnRetry
                         flip tryRunAction (return()) $ do
                           action >>= putMVar mvarActionResult
                           Element.setClassName noConnBox (unSel selHidden)
                         flip Gtk.timeoutAdd 3000 (setStartingState htmlBtnRetry >> return False) -}
