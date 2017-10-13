{-# LANGUAGE OverloadedStrings #-}

module Graphics.Data.Forms
  ( formSignIn, errInvalidData ) where

import Graphics.Form
import Graphics.Data.Selectors

pressEnterAdvice :: DescToken
pressEnterAdvice = OneLineDescText "Нажмите Enter для продолжения."

formSignIn :: UIFormBuilder

-- Sign In ---------------------------------------------------------------------

formSignIn = UIFormBuilder "Вход в систему"
  [ SwitchBtnToken (unSel selSwitchFst) "РЕГИСТРАЦИЯ"
  , SwitchBtnToken (unSel selSwitchSnd) "ВОССТАНОВЛЕНИЕ" ]

  [ OneLineDescText "Введите действительные данные,"
  , OneLineDescText "связанные с вашим аккаунтом."
  , pressEnterAdvice ]

  [ SingleField (unSel selInpEmail) "Email"
  , SingleField (unSel selInpPassw) "Пароль" ]

errInvalidData :: [DescToken]
errInvalidData = [ OneLineDescText "Несуществующий аккаунт, либо"
                 , OneLineDescText "неправильный пароль."
                 , OneLineDescText "Убедитесь, что вы ввели"
                 , OneLineDescText "корректные данные и"
                 , OneLineDescText "попробуйте еще раз."]
