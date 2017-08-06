{-# LANGUAGE RecordWildCards #-}

module GUI.Elements.Buttons
  ( BtnEvents(..)
  , importantBtn
  , linkBtn ) where

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events

data BtnEvents a = BtnEvents
  { btnClick :: UI a
  , btnHover :: UI a
  , btnLeave :: UI a }

class EventsClass t where
  setEvents :: Element -> t -> UI ()

instance EventsClass (BtnEvents a) where
  setEvents btn BtnEvents{..} = do
    on Events.click btn $ const $ btnClick
    on Events.hover btn $ const $ btnHover
    on Events.leave btn $ const $ btnLeave

importantBtn :: String -> UI Element
importantBtn btnText =
  let greenColor      = "#5F9EA0"
      lightGreenColor = "#66A9AB"
      darkGreenColor  = "#5a9597"
      state color     = set style [ ("background", color) ]
      primaryState    = state greenColor
      focusState      = state lightGreenColor
      pressedState    = state darkGreenColor
  in do
    btn <- Elems.button # set text btnText
                        # set style [ ("background", greenColor)
                                    , ("color", "white")
                                    , ("width", "300px")
                                    , ("height", "44px")
                                    , ("border-radius", "0px")
                                    , ("border-width", "0px")
                                    , ("font-family", "PT Sans")
                                    , ("font-size", "16px")
                                    , ("outline",  "none") ]
    setEvents btn $ BtnEvents
       ( do element btn # pressedState )
       ( do element btn # focusState )
       ( do element btn # primaryState )
    return btn

linkBtn :: String -> UI Element
linkBtn btnText =
  let gray = "#808080"
  in do
    btn <- Elems.button # set text btnText
                        # set style [ ("background", "white")
                                    , ("border-width", "0px")
                                    , ("text-decoration", "underline")
                                    , ("outline", "none")
                                    , ("font-family", "PT Sans")
                                    , ("color", "gray")
                                    , ("-webkit-user-select", "none") ]
    setEvents btn $ BtnEvents
      ( do element btn )
      ( do element btn # set style [ ("text-decoration", "none") ] )
      ( do element btn # set style [ ("text-decoration", "underline") ] )
    return btn
