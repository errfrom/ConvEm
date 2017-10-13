{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.General
  ( Id, Class
  , selNonexistent, operateElemById
  , onClick, onFocus, onPress
  , getValue ) where

import Data.Text                              (Text, unpack)
import Control.Monad.IO.Class                 (liftIO)
import Control.Monad.Trans.Reader             (ReaderT(..), ask)
import Graphics.UI.Gtk.WebKit.DOM.EventTarget (EventTargetClass, addEventListener)
import Graphics.UI.Gtk.WebKit.DOM.MouseEvent  (MouseEvent)
import Graphics.UI.Gtk.WebKit.DOM.EventTargetClosures
import Graphics.UI.Gtk.WebKit.DOM.Element     (Element)
import Graphics.UI.Gtk.WebKit.DOM.Document    (DocumentClass)
import Graphics.Data.Selectors                (CSSSel, unSel)
import System.Glib.Signals                    (ConnectId)
import Graphics.UI.Gtk.Abstract.Widget        (WidgetClass, onKeyRelease)
import Graphics.UI.Gtk.Gdk.Events             (Event(Key))
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as Inp
import qualified Graphics.UI.Gtk.WebKit.DOM.Document         as Doc

type Id    = Text
type Class = Text

selNonexistent :: Text -> IO ()
selNonexistent sel = putStrLn $ "Not valid selector - " ++ (unpack sel) ++ "."

onMouseEvent :: (EventTargetClass self) => String -> self -> IO () -> IO ()
onMouseEvent eventName target action = do
  eventListener <- eventListenerNew $ \(_ :: MouseEvent) -> action
  addEventListener target eventName (Just eventListener) True

onClick, onFocus :: (EventTargetClass self) => self -> IO () -> IO ()

onClick = onMouseEvent "click"
onFocus = onMouseEvent "focus"

-- Устанавливает событие, происходящие при нажатии на конкретную клавишу.
onPress :: (WidgetClass self) => self -> Text -> IO () -> IO (ConnectId self)
onPress widget eventKeyName' action =
  let event (Key _ _ _ _ _ _ _ _ eventKeyName _)
       |eventKeyName == eventKeyName' = action >> return True
       |otherwise                     = return False
      event _                         = return False
  in onKeyRelease widget event

-- Обобщенное продолжение, свойственное любой функции,
-- каким-либо образом оперирующей с одним элементом.
-- TODO: Повесить логгер.
operateElemById :: (DocumentClass doc) => CSSSel -> (Element -> IO ()) -> ReaderT doc IO ()
operateElemById selId behavior = do
  doc <- ask
  liftIO $ do
    let selId' = unSel selId
    el <- Doc.getElementById doc selId'
    maybe (selNonexistent selId') behavior el

getValue :: (DocumentClass doc) => doc -> CSSSel -> IO (Maybe String)
getValue doc selId =
  let selId' = unSel selId
  in do
    inp <- Doc.getElementById doc selId'
    maybe (selNonexistent selId' >> return Nothing)
          (Inp.getValue . Inp.castToHTMLInputElement) inp
