module Main.Login.GUI.Auth
  (handleAuth) where

--------------------------------------------------------------------------------
-- Обрабатывает события авторизации.
-- Отображает результаты этих событий графически.
--------------------------------------------------------------------------------

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Events   as Events
import qualified Data.Int                        as Int (Int64)
import qualified Data.Maybe                      as M   (fromJust)
import Network.Socket                                   (Socket)
import Control.Monad                                    (void)
import qualified Utils                                  (getElemById
                                                        ,removeClass)
import Main.Login.Logic.Auth                            (auth)
import Types.Results                                    (MistakeIn(..)
                                                        ,AuthResult(..))


-- | При неудачной авторизации,
-- графически уведомляет пользователя
-- какие действия следует предпринять для
-- успешного входа.
handleAuth :: Socket -> UI()
handleAuth sock = do
  invalidInpBox <- Utils.getElemById "invalid-input-text"
  inpEmail      <- Utils.getElemById "inp-email"
  inpPassw      <- Utils.getElemById "inp-passw"
  let onFocusRemoveErrClass = onFocusRemoveErrClass' invalidInpBox
      showErrText           = showErrText' invalidInpBox
  loginResult <- auth sock inpEmail inpPassw
  mapM_ onFocusRemoveErrClass [ inpEmail, inpPassw ]
  case loginResult of
    CorrectPassword     -> return () -- TODO: Переход к основному окну.
    IncorrectPassword   -> void $ showErrText "Введенный пароль неверен."
    ANonexistentAccount -> void $ showErrText "Аккаунта с таким Email не существует."
    BlockedAccount      -> void $ showErrText "Аккаунт заблокирован."
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
