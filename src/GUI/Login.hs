{-# LANGUAGE RecordWildCards #-}

module GUI.Login where

--------------------------------------------------------------------------------
-- Описывает форму входа.
--------------------------------------------------------------------------------

--Threepenny--------------------------------------------------------------------
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events
--GUI---------------------------------------------------------------------------
import qualified GUI.Elements.Button as Btn
import qualified GUI.Elements.Input  as Inp (InputType(..), simpleInput)
--Logic-------------------------------------------------------------------------
import qualified Logic.Login as Login (login)
import           Logic.Login          (MistakeIn(..), LoginResult(..))
--Other-------------------------------------------------------------------------
import qualified Utils                (removeClass)
import qualified Data.Int      as Int (Int64)
import qualified Data.Char     as Ch  (isDigit, isUpper)
import           Control.Monad        (void)
--------------------------------------------------------------------------------

loginForm :: UI Element
loginForm = do
  regBtn         <- Btn.linkBtn "Регистрация"
  forgotBtn      <- Btn.linkBtn "Забыли пароль?"
  additionalBtns <- Elems.div # set children [ regBtn, forgotBtn ]
                              # set (attr "class") "additional-btns"
  inpEmail    <- Inp.simpleInput "E-mail" Inp.Simple
  inpPassword <- Inp.simpleInput "Пароль" Inp.Password
  btnLogin    <- Btn.importantBtn "Войти"
  on Events.click btnLogin $ \_ -> (handleLogin inpEmail inpPassword)
  form <- Elems.div # set children [ inpEmail
                                   , inpPassword
                                   , btnLogin
                                   , additionalBtns ]
                    # set (attr "class") "main-div"
  Elems.center # set children [ form ]

-- | При неудачной авторизации,
-- графически уведомляет пользователя
-- какие действия следует предпринять для
-- успешного входа.
handleLogin :: Element -> Element -> UI()
handleLogin inpEmail inpPassword = do
  loginResult <- Login.login inpEmail inpPassword
  mapM_ onFocusRemoveErrClass [ inpEmail, inpPassword ]
  case loginResult of
    InvalidValues mistakeIn ->
      case mistakeIn of
        InEmailField    -> void (setErrClass inpEmail)
        InPasswordField -> void (setErrClass inpPassword)
        InBothFields    -> mapM_ setErrClass [ inpEmail, inpPassword ]
  return ()
  where errClass                 = "with-error"
        setErrClass el           = (return el) # set (attr "class") errClass
        onFocusRemoveErrClass el =
          on Events.focus el $ \_ -> Utils.removeClass el errClass


-- Move to registration --------------------------------------------------------
type Advice   = String
type Password = String

checkPassword :: Password -> [Advice]
checkPassword password = worker password [checkLength, checkNumbers, checkUpper]
 where worker :: Password -> [(Password -> Maybe Advice)] -> [Advice]
       worker _ [] = []
       worker password (fun:funcs) =
         case (fun password) of
           Just advice -> advice : (worker password funcs)
           Nothing     -> [] ++ (worker password funcs)

       checkLength password
        |length password >= 9 = Nothing
        |otherwise            = Just "быть не короче 9 символов."
       checkNumbers password
        |length (filter Ch.isDigit password) >= 2 = Nothing
        |otherwise                                = Just "содержать не менее 2 цифр."
       checkUpper password
        |length (filter Ch.isUpper password) >= 1 = Nothing
        |otherwise                                = Just "содержать заглваную букву."
-- Move to registration --------------------------------------------------------
