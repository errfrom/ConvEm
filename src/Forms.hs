module Forms where

--------------------------------------------------------------------------------
-- Содержит все GUI формы, а также осуществляет перемещение между оными.
-- Связывает GUI и Logic.
--------------------------------------------------------------------------------


import Graphics.UI.Threepenny.Core               hiding (row)
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events
import           GUI.General
import qualified GUI.Login   as GUILogin
import           Server.General

data Stage =
  Auth
 |Reg
 |Sync
 |Recovery
 |Main

-- | Форма авторизации.
loginForm :: UI Element
loginForm = do
  window <- askWindow
  sock   <- liftIO (initSocket ClientSocket)
  build "login-form"
    [ wrap [ add (LblHeader "Авторизация")
           , add (LblDesc   "Введите ваш E-mail и пароль для продолжения работы.") ]
    , wrap [ add (InpSimple "E-mail"  ) `as` "inp-email" ]
    , wrap [ add (InpPassword "Пароль") `as` "inp-passw" ]
    , add LblInvalid
    , wrap [ add (BtnImportant "Вперед") `bind` (GUILogin.handleLogin sock window)
           , additional [ add (BtnLink "Регистрация"   ) `switch` Reg
                        , add (BtnLink "Забыли пароль?") `switch` Recovery ]]]

-- | Форма регистрации.
regForm :: UI Element
regForm = do
  window <- askWindow
  build "reg-form"
    [ wrap [ add (LblHeader "Регистрация")
           , add (LblDesc   "Введите необходимые данные для начала работы.") ]
    , row  [ (short . add) (InpSimple "Имя"               ) `as` "inp-name"
           , (short . add) (InpSimple "Фамилия"           ) `as` "inp-surname" ]
    , wrap [  add          (InpSimple "E-mail"            ) `as` "inp-email"   ]
    , row  [ (short . add) (InpPassword "Пароль"          ) `as` "inp-passw"
           , (short . add) (InpPassword "Повторите пароль") `as` "inp-repeat"  ]
    , add LblInvalid
    , wrap [ add (BtnImportant "Готово") `bind` return()
           , additional [ add (BtnLink "Вернуться назад") `switch` Auth ]]]

-- | Форма восстановления пароля.
recoveryForm :: UI Element
recoveryForm = do
  window <- askWindow
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
