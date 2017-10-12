{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.General
  ( Id, Class
  , selNonexistent, operateElemById
  , onClick, onPress ) where

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
import qualified Graphics.UI.Gtk.WebKit.DOM.Document as Doc

type Id    = Text
type Class = Text

selNonexistent :: Text -> IO ()
selNonexistent sel = putStrLn $ "Not valid selector - " ++ (unpack sel) ++ "."

-- Устанавливает событие, происходящие при нажатии на данный элемент.
onClick :: (EventTargetClass self) => self -> IO () -> IO ()
onClick target action =
  let eventClick = "click"
  in do
    eventListener <- eventListenerNew $ \(_ :: MouseEvent) -> action
    addEventListener target eventClick (Just eventListener) True

-- Устанавливает событие, происходящие при нажатии на конкретную клавишу.
onPress :: (WidgetClass self) => self -> Text -> IO () -> IO (ConnectId self)
onPress widget eventKeyName action =
  let event (Key _ _ _ _ _ _ _ _ eventKeyName _) = action >> return True
      event _                                    = return False
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
