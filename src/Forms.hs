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
import           Server.General

data Stage =
  Auth     -- Авторизации
 |Reg      -- Регистрация
 |Sync     -- Синхронизация
 |Recovery -- Восстановление
 |Main     -- Главное окно

-- | Форма авторизации.
loginForm :: Window -> UI Element
loginForm window = do
  sock <- liftIO (initSocket ClientSocket)
  build "login-form"
    [ wrap [ add (LblHeader "Авторизация")
           , add (LblDesc   "Введите ваш E-mail и пароль для продолжения работы.") ]
    , wrap [ add (InpSimple "E-mail"  ) `as` "inp-email" ]
    , wrap [ add (InpPassword "Пароль") `as` "inp-passw" ]
    , add LblInvalid
    , wrap [ add (BtnImportant "Вперед") `bind` (GUILogin.handleLogin sock window)
           , additional [ add (BtnLink "Регистрация"   ) `switch` Reg
                        , add (BtnLink "Забыли пароль?") `switch` Recovery ]]]

-- | Форма восстановления пароля.
recoveryForm :: Window -> UI Element
recoveryForm window = do
  build "recovery-form"
    [ wrap [ add (LblHeader "Восстановление пароля")
           , add (LblDesc "На ваш E-mail будет отправлен ключ для смены пароля.") ]
    , wrap [ add (InpSimple "E-mail") `as` "inp-email" ]
    , add LblInvalid
    , wrap [ add (BtnImportant "Отправить") `bind` return ()
           , additional [ add (BtnLink "Вернуться назад") `switch` Auth
                        , add (BtnLink "Не пришел ключ?") ]]]

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
