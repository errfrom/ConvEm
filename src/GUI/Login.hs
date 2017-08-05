{-# LANGUAGE RecordWildCards #-}

module GUI.Login where

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG.Attributes as SVG
import qualified Graphics.UI.Threepenny.Elements       as Elems
import qualified Graphics.UI.Threepenny.Events         as Events
import qualified Control.Concurrent.Timer              as Timer   (oneShotTimer)
import qualified Control.Concurrent.Suspend.Lifted     as Suspend (sDelay)
import qualified Data.Int                              as Int     (Int64)

data BtnEvents a = BtnEvents
  { btnClick :: UI a
  , btnHover :: UI a
  , btnLeave :: UI a }

data InputType =
  Simple
 |Password

logo :: UI Element
logo = Elems.img # set (attr "src") "file:/resources/logo.svg"
                 # set (attr "alt") "Diregd logo"

setBtnEvents :: Element -> BtnEvents Element -> UI()
setBtnEvents btn BtnEvents{..} = do
  on Events.click btn $ const $ btnClick
  on Events.hover btn $ const $ btnHover
  on Events.leave btn $ const $ btnLeave

loginForm :: UI Element
loginForm = do
  regBtn         <- linkBtn "Регистрация"
  forgotBtn      <- linkBtn "Забыли пароль?"
  additionalBtns <- Elems.div # set children [ regBtn, forgotBtn ]
                              # set style [("text-align", "center")]
  col <- column [ logo
                , (inputLine "Email" Simple)
                , (inputLine "Пароль" Password)
                , (importantBtn "Войти")
                , (return additionalBtns) ] # set style [ ("margin", "0 auto")
                                                        , ("overflow", "auto") ]
  Elems.div # set children [col]

inputLine :: String -> InputType -> UI Element
inputLine placeHolder inputType =
  let gray    = "#808080"
      inpType = case inputType of
                  Simple   -> "simple"
                  Password -> "password"
  in do
  Elems.input # set text "Email"
              # set (attr "placeholder") placeHolder
              # set (attr "type") inpType
              # set style [ ("width", "300px")
                          , ("height", "40px")
                          , ("line-height", "24px")
                          , ("border-width", "0px")
                          , ("font-family", "PT Sans")
                          , ("color", gray)
                          , ("border-radius", "4px")
                          , ("outline",  "none") ]

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
                                    , ("color", "gray") ]
    setBtnEvents btn (BtnEvents
      { btnClick = do element btn
      , btnHover = do element btn # set style [ ("text-decoration", "none") ]
      , btnLeave = do element btn # set style [ ("text-decoration", "underline") ] } )
    return btn

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
    setBtnEvents btn (BtnEvents
      { btnClick = do element btn # pressedState
      , btnHover = do element btn # focusState
      , btnLeave = do element btn # primaryState })
    return btn
