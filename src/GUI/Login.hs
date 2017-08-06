{-# LANGUAGE RecordWildCards #-}

module GUI.Login where

--------------------------------------------------------------------------------
-- Описывает форму входа.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements       as Elems
import qualified Graphics.UI.Threepenny.Events         as Events
import qualified Control.Concurrent.Timer              as Timer   (oneShotTimer)
import qualified Control.Concurrent.Suspend.Lifted     as Suspend (sDelay)
import qualified Data.Int                              as Int     (Int64)
import qualified GUI.Elements.Buttons                  as Btns
--------------------------------------------------------------------------------

data InputType =
  Simple
 |Password

logo :: UI Element
logo = Elems.img # set (attr "src") "file:/resources/logo.svg"
                 # set (attr "alt") "Diregd logo"

loginForm :: UI Element
loginForm = do
  regBtn         <- Btns.linkBtn "Регистрация"
  forgotBtn      <- Btns.linkBtn "Забыли пароль?"
  additionalBtns <- Elems.div # set children [ regBtn, forgotBtn ]
                              # set style [ ("text-align", "center")
                                          , ("margin", "auto") ]
  inpEmail    <- inputLine "Email" Simple
  inpPassword <- inputLine "Пароль" Password
  btnLogin    <- Btns.importantBtn "Войти"
  form <- Elems.div # set children [ inpEmail
                                   , inpPassword
                                   , btnLogin
                                   , additionalBtns ]
                    # set style [ ("-webkit-user-select", "none")
                                , ("width", "300px")
                                , ("height", "400px")
                                , ("margin", "auto")
                                , ("position", "absolute")
                                , ("top", "20%")
                                , ("left", "0")
                                , ("right", "0")
                                , ("bottom", "0") ]
  Elems.center # set children [ form ]

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
