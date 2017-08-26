module Main.Login.GUI.Forms {-# DEPRECATED "Will be removed and decentralized soon" #-} where

--------------------------------------------------------------------------------
-- Содержит все GUI формы, а также осуществляет перемещение между оными.
-- Связывает GUI и Logic.
--------------------------------------------------------------------------------


import Graphics.UI.Threepenny.Core             hiding    (row)
import qualified Graphics.UI.Threepenny.Events as Events (click)
import Network.Socket                                    (Socket)
import qualified Server.General                as Server (initServer, initSocket)
import Types.General                                     (RecStage(..), Stage(..))
import Types.Server                                      (SocketType(..))
import Main.Login.GUI.Auth                               (handleAuth)
import Main.Login.GUI.General

switch :: Socket -> Stage -> UI Element -> UI Element
switch sock stage el = el

  {- el >>= \el' -> worker el' >> return el'
    where worker el = on Events.click el $ \_ -> do
            window <- liftIO (getWindow el)
            clearWindow window
            getBody window #+ [ form stage sock ]

          clearWindow window = do
            [mainDiv] <- getElementsByClassName window "main-div"
            [center]  <- getElementsByTagName   window "center"
            mapM_ delete [ mainDiv, center ] -}

type Form = Socket -> UI Element
startForm, authForm, regForm, recoveryForm_Email :: Form

--Начальное окно----------------------------------------------------------------
startForm sock = do
  window <- askWindow
  build "start-form"
   [ wrap [ add (LblHeader "Приветствуем вас!")
          , add (LblDesc   "Извольте насладиться ощущением прогрессивного общения.") ]
   , wrap [ switch sock Auth (add $ BtnImportant "Вперед") ]]

--Авторизация-------------------------------------------------------------------
authForm sock = do
  window <- askWindow
  build "login-form"
    [ wrap [ add (LblHeader "Авторизация")
           , add (LblDesc   "Введите ваш E-mail и пароль для продолжения работы.") ]
    , wrap [ add (InpSimple "E-mail"  ) `as` "inp-email" ]
    , wrap [ add (InpPassword "Пароль") `as` "inp-passw" ]
    , add LblInvalid
    , wrap [ add (BtnImportant "Вперед") `bind` (handleAuth sock)
           , additional [ switch sock Reg (add $ BtnLink "Регистрация")
                        , switch sock (Recovery SendingEmail)
                                      (add $ BtnLink "Забыли пароль?") ]]]

--Регистрация----*---------------------------------------------------------------
regForm sock = do
  window <- askWindow
  build "reg-form"
    [ wrap [ add (LblHeader "Регистрация")
           , add (LblDesc   "Введите необходимые данные для начала работы.") ]
    , wrap [  add       (InpSimple "E-mail"            ) `as` "inp-email"   ]
    , row (short $ (add (InpSimple "Имя"               ) `as` "inp-name"))
          (short $ (add (InpSimple "Фамилия"           ) `as` "inp-surname"))
    , row (short $ (add (InpPassword "Пароль"          ) `as` "inp-passw"))
          (short $ (add (InpPassword "Повторите пароль") `as` "inp-repeat"))
    , add LblInvalid
    , wrap [ add (BtnImportant "Готово") `bind` return()
           , additional [ switch sock Auth (add $ BtnLink "Вернуться назад")
                        , add (BtnLink "Синхронизация") ]]]

--Восстановление пароля(Email)--------------------------------------------------
recoveryForm_Email sock = do
  window <- askWindow
  build "recovery-form"
    [ wrap [ add (LblHeader "Восстановление пароля")
           , add (LblDesc "На ваш E-mail будет отправлен ключ для смены пароля.") ]
    , wrap [ add (InpSimple "E-mail") `as` "inp-email" ]
    , add LblInvalid
    , wrap [ add (BtnImportant "Отправить") `bind` return ()
           , additional [ switch sock Auth (add $ BtnLink "Вернуться назад")
                        , add (BtnLink "Не пришел ключ?") ]]]
