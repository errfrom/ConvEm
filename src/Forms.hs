module Forms where

--------------------------------------------------------------------------------
-- Содержит все GUI формы, а также осуществляет перемещение между оными.
-- Связывает GUI и Logic.
--------------------------------------------------------------------------------

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events

import           GUI.General
import qualified GUI.Login   as GUILogin

data Stage =
  Auth     -- Авторизации
 |Reg      -- Регистрация
 |Sync     -- Синхронизация
 |Recovery -- Восстановление
 |Main     -- Главное окно

-- | Форма авторизации.
loginForm :: Window -> UI Element
loginForm window = do
  build "login-form"
    [ wrap [ hdrText  "Авторизация"
           , formText "Введите ваш E-mail и пароль для продолжения работы." ]
    , wrap [ sInp "E-mail" `as` "inp-email" ]
    , wrap [ pInp "Пароль" `as` "inp-passw" ]
    , invalidBox
    , wrap [ btnImportant "Вперед" `bind` (GUILogin.handleLogin window)
           , additional [ btnLink "Регистрация"    `switch` Reg
                        , btnLink "Забыли пароль?" `switch` Recovery ]]]

-- | Форма восстановления пароля.
recoveryForm :: Window -> UI Element
recoveryForm window = do
  build "recovery-form"
    [ wrap [ hdrText  "Восстановление пароля"
           , formText "На ваш E-mail будет отправлен ключ для смены пароля." ]
    , wrap [ sInp "E-mail" `as` "inp-email" ]
    , invalidBox
    , wrap [ btnImportant "Отправить" `bind` return ()
           , additional [ btnLink "Вернуться назад" `switch` Auth
                        , btnLink "Не пришел ключ?" ]]]

switch :: UI Element -> Stage -> UI Element
switch el' stage = do
  el <- el'
  on Events.click el $ \_ -> do
    window    <- liftIO (getWindow el)
    [mainDiv] <- getElementsByClassName window "main-div"
    [center]  <- getElementsByTagName   window "center"
    delete mainDiv
    delete center -- FIXME: Not the best way to clear body.
    getBody window #+ [ (getForm stage) window ]
  return el
  where getForm stage =
          case stage of
            Auth     -> loginForm
            Recovery -> recoveryForm
