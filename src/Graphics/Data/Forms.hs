{-# LANGUAGE OverloadedStrings #-}

module Graphics.Data.Forms
  ( formSignIn
  , formEmailRecovery
  , errInvalidData, errInvalidEmail ) where

import Graphics.Form
import Graphics.Data.Selectors

type ErrorText = [DescToken]

pressEnterAdvice :: DescToken
pressEnterAdvice = OneLineDescText "Нажмите Enter для продолжения."

formSignIn, formEmailRecovery :: UIFormBuilder

formSignIn = UIFormBuilder "Вход в систему"
  [ SwitchBtnToken (unSel selSwitchFst) "Восстановление"
  , SwitchBtnToken (unSel selSwitchSnd) "Создать аккаунт" ]

  [ OneLineDescText "Введите действительные данные,"
  , OneLineDescText "связанные с вашим аккаунтом."
  , pressEnterAdvice ]

  [ SingleField (unSel selInpEmail) "Email"
  , SingleField (unSel selInpPassw) "Пароль" ]

formEmailRecovery = UIFormBuilder "Восстановление"
  [ SwitchBtnToken (unSel selSwitchFst) "Назад"
  , SwitchBtnToken (unSel selSwitchSnd) "Создать аккаунт" ]

  [ OneLineDescText "Введите Email, на который"
  , OneLineDescText "впоследствие будет отправлен"
  , OneLineDescText "ключ восстановления доступа."
  , pressEnterAdvice ]

  [ SingleField (unSel selInpEmail) "Email" ]

errInvalidData, errInvalidEmail :: ErrorText

errInvalidData = [ OneLineDescText "Несуществующий аккаунт, либо"
                 , OneLineDescText "неправильный пароль."
                 , OneLineDescText "Убедитесь, что вы ввели"
                 , OneLineDescText "корректные данные и"
                 , OneLineDescText "попробуйте еще раз."]

errInvalidEmail = [ OneLineDescText "Введенный вами Email либо"
                  , OneLineDescText "неправильный, либо не"
                  , OneLineDescText "зарегистрирован в системе." ]
