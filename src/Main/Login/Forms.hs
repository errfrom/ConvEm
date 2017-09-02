{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main.Login.Forms
  ( initForms ) where

import Graphics.UI.Threepenny.Core hiding    (row)
import qualified Server.General    as Server (initSocket)
import Types.General                         (SocketType(..))
import Types.Hierarchy
import Types.Results
import Types.Elems
import Main.Login.GUI
import Main.Login.Auth


-- | Инициализирует клиентский сокет и начальную форму.
-- Последущие формы чередуются функцией switch.
-- Также позволяет применять оформление одновременно ко
-- всем формам.
initForms :: UI Element
initForms = do
  sock <- liftIO (Server.initSocket ClientSocket)
  win  <- askWindow
  getBody win #+ [ add (ImgHeader "header.png")
                 , nonfunForm Start sock ]

instance NonfunForm Start where
  nonfunForm Start sock = do
    build "start-form"
     [ wrap [ add (LblHeader "Приветствуем вас!")
            , add (LblDesc   "Извольте насладиться ощущением прогрессивного общения.") ]
     , wrap [ switch sock Auth (add $ BtnImportant "Вперед") ]]

instance Form Auth AuthResult where
  form Auth sock =
    build "login-form"
      [ wrap [ add (LblHeader   "Авторизация")
             , add (LblDesc     "Введите ваш E-mail и пароль для продолжения работы.") ]
      , wrap [ add (InpEmail    "E-mail")    `as` "inp-email" ]
      , wrap [ add (InpPassword "Пароль") `as` "inp-passw" ]
      , add LblInvalid
      , wrap [ add (BtnImportant "Вперед") `bind` (handle Auth sock)
             , additional [ add (BtnLink "Регистрация")
                          , add (BtnLink "Забыли пароль?") ]]]

instance NonfunForm Reg where
  nonfunForm _ sock = do
    build "reg-form"
      [ wrap [ add (LblHeader "Регистрация")
             , add (LblDesc   "Введите необходимые данные для начала работы.") ]
      , wrap [  add       (InpSimple   "E-mail"           ) `as` "inp-email"   ]
      , row (short $ (add (InpSimple   "Имя"              ) `as` "inp-name"   ))
            (short $ (add (InpSimple   "Фамилия"          ) `as` "inp-surname"))
      , row (short $ (add (InpPassword "Пароль"           ) `as` "inp-passw"  ))
            (short $ (add (InpPassword "Повторите пароль" ) `as` "inp-repeat" ))
      , add LblInvalid
      , wrap [ add (BtnImportant "Готово") `bind` return()
             , additional [ add $ BtnLink "Вернуться назад"
                          , add (BtnLink "Синхронизация") ]]]


instance NonfunForm Recovery where
  nonfunForm (Recovery SendingEmail) sock = do
    window <- askWindow
    build "recovery-form"
      [ wrap [ add (LblHeader "Восстановление пароля")
             , add (LblDesc "На ваш E-mail будет отправлен ключ для смены пароля.") ]
      , wrap [ add (InpSimple "E-mail") `as` "inp-email" ]
      , add LblInvalid
      , wrap [ add (BtnImportant "Отправить") `bind` return ()
             , additional [ add $ BtnLink "Вернуться назад"
                          , add $ BtnLink "Не пришел ключ?" ]]]
