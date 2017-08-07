{-# LANGUAGE RecordWildCards #-}

module GUI.Login where

--------------------------------------------------------------------------------
-- Описывает форму входа.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements       as Elems
import qualified Graphics.UI.Threepenny.Events         as Events
import qualified Data.Int                              as Int     (Int64)
import qualified GUI.Elements.Button                   as Btn
import qualified GUI.Elements.Input                    as Inp     (InputType(..)
                                                                  ,simpleInput)
--------------------------------------------------------------------------------

loginForm :: UI Element
loginForm = do
  regBtn         <- Btn.linkBtn "Регистрация"
  forgotBtn      <- Btn.linkBtn "Забыли пароль?"
  additionalBtns <- Elems.div # set children [ regBtn, forgotBtn ]
                              # set (attr "class") "additional-btns"
  inpEmail    <- Inp.simpleInput "Email"  Inp.Simple
  inpPassword <- Inp.simpleInput "Пароль" Inp.Password
  btnLogin    <- Btn.importantBtn "Войти"
  form <- Elems.div # set children [ inpEmail
                                   , inpPassword
                                   , btnLogin
                                   , additionalBtns ]
                    # set (attr "class") "main-div"
  Elems.center # set children [ form ]
