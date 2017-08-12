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

loginForm :: Window -> UI Element
loginForm window = do
  build "login-form"
    [ wrap [ hdrText  "Авторизация"
           , formText "Введите ваш Email и пароль для продолжения работы." ]
    , wrap [ sInp "E-mail" `as` "inp-email" ]
    , wrap [ pInp "Пароль" `as` "inp-passw" ]
    , invalidBox
    , wrap [ btnImportant "Вперед" `bind` (GUILogin.handleLogin window)
           , additional [ btnLink "Регистрация"    `switch` Reg
                        , btnLink "Забыли пароль?" `switch` Recovery ]]]

switch :: UI Element -> Stage -> UI Element
switch el' stage = do
  el     <- el'
  on Events.click el $ \_ -> do
    window <- liftIO (getWindow el)
    body   <- getBody window
    delete body
  el'
