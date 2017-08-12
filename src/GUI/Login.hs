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
import           GUI.General (getElemById)
--Logic-------------------------------------------------------------------------
import qualified Logic.Login as Login (login)
import           Logic.Login          (MistakeIn(..), LoginResult(..))
--Other-------------------------------------------------------------------------
import qualified Utils                (removeClass)
import qualified Data.Int      as Int (Int64)
import qualified Data.Char     as Ch  (isDigit, isUpper)
import qualified Data.Maybe    as M   (fromJust)
import           Control.Monad        (void)
--------------------------------------------------------------------------------

-- | При неудачной авторизации,
-- графически уведомляет пользователя
-- какие действия следует предпринять для
-- успешного входа.
handleLogin :: Window -> UI()
handleLogin window = do
  invalidInpBox <- getElemById window "invalid-input-text"
  inpEmail      <- getElemById window "inp-email"
  inpPassw      <- getElemById window "inp-passw"
  let onFocusRemoveErrClass = onFocusRemoveErrClass' invalidInpBox
      showErrText           = showErrText' invalidInpBox
  loginResult <- Login.login inpEmail inpPassw
  mapM_ onFocusRemoveErrClass [ inpEmail, inpPassw ]
  case loginResult of
    CorrectPassword    -> return () -- TODO: Переход к основному окну.
    IncorrectPassword  -> void $ showErrText "Введенный пароль неверен."
    NonexistentAccount -> void $ showErrText "Аккаунта с таким Email не существует."
    BlockedAccount     -> void $ showErrText "Аккаунт заблокирован."
    InvalidValues mistakeIn -> do
      case mistakeIn of
        InEmailField -> do
          showErrText "Введите корректный E-mail адрес."
          void (setErrClass inpEmail)
        InPasswordField -> do
          showErrText "Введите корректный пароль."
          void (setErrClass inpPassw)
        InBothFields -> do
          showErrText "Введите корректные данные."
          mapM_ setErrClass [ inpEmail, inpPassw ]
  return ()
  where errClass                = "with-error"
        setErrClass el          = (return el) # set (attr "class") errClass
        showErrText' inpBox msg = (return inpBox) # set (attr "class") "with-error"
                                                  # set text msg
        onFocusRemoveErrClass' inpBox el =
          on Events.focus el $ \_ ->
            mapM_ (flip Utils.removeClass errClass) [ el, inpBox ]

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
