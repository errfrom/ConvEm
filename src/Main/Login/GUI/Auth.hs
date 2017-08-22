module Main.Login.GUI.Auth where

--------------------------------------------------------------------------------
-- Обрабатывает события авторизации.
-- Отображает результаты этих событий графически.
--------------------------------------------------------------------------------

--Threepenny--------------------------------------------------------------------
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events
--GUI---------------------------------------------------------------------------
import GUI.General (getElemById)
--Logic-------------------------------------------------------------------------
import qualified Logic.Login as Login (login)
import           Logic.General        (LoginResult(..))
import           Logic.Login          (MistakeIn(..))
--Other-------------------------------------------------------------------------
import qualified Utils                (removeClass)
import qualified Data.Int      as Int (Int64)
import qualified Data.Maybe    as M   (fromJust)
import           Control.Monad        (void)
--------------------------------------------------------------------------------

-- | При неудачной авторизации,
-- графически уведомляет пользователя
-- какие действия следует предпринять для
-- успешного входа.
handleLogin :: UI()
handleLogin sock = do
  window        <- askWindow
  invalidInpBox <- getElemById window "invalid-input-text"
  inpEmail      <- getElemById window "inp-email"
  inpPassw      <- getElemById window "inp-passw"
  let onFocusRemoveErrClass = onFocusRemoveErrClass' invalidInpBox
      showErrText           = showErrText' invalidInpBox
  loginResult <- Login.login sock inpEmail inpPassw
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
