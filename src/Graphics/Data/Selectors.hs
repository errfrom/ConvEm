{-# LANGUAGE OverloadedStrings #-}

module Graphics.Data.Selectors
  ( unSel, CSSSel
  , selInpEmail, selInpPassw, selInpPasswRepeat, selInpName, selInpSurname
  , selBtnRules, selTxtHeader, selBoxAdvice, selBoxInputs, selBoxError, selForm
  , selBtnLink, selTxtAdvice, selBoxFieldPair, selBoxFieldFirst
  , selBoxFieldSecond, selShowError, selBtnQuit, selBoxNavigation
  , selBtnReg, selBtnRecovery ) where

import Graphics.General (Id, Class)

-- CSSSel и ацессор unSel дает гарантию того, что
-- селектор не определен за пределами этого модуля.
newtype CSSSel a = CSSSel a

unSel :: CSSSel a -> a
unSel (CSSSel sel) = sel

-- Id --------------------------------------------------------------------------

selInpEmail, selInpPassw, selInpPasswRepeat, selInpName, selInpSurname
           , selBtnRules, selTxtHeader, selBoxAdvice, selBoxInputs
           , selBoxError, selForm, selBtnQuit, selBoxNavigation
           , selBtnReg, selBtnRecovery  :: CSSSel Id

selForm                              = CSSSel "form"

selInpEmail                          = CSSSel "inp-email"
selInpPassw                          = CSSSel "inp-passw"
selInpPasswRepeat                    = CSSSel "inp-passw-repeat"
selInpName                           = CSSSel "inp-name"
selInpSurname                        = CSSSel "inp-surname"

-- Кнопка, при нажатии на которую появляются подсказки пользователю,
-- указывающие, как следует вводить данные при регистрации.
selBtnRules                          = CSSSel "btn-rules"
selBtnQuit                           = CSSSel "btn-sign-out"
selBtnReg                            = CSSSel "btn-switch-reg"
selBtnRecovery                       = CSSSel "btn-switch-recovery"

-- Динамические контейнеры, находящиеся в любой форме.
selTxtHeader                         = CSSSel "header-text"
selBoxNavigation                     = CSSSel "navigation-box"
selBoxError                          = CSSSel "error-box"
selBoxAdvice                         = CSSSel "advice-box"
selBoxInputs                         = CSSSel "inputs"

-- Class -----------------------------------------------------------------------

selBtnLink, selTxtAdvice, selBoxFieldPair, selBoxFieldFirst
          , selBoxFieldSecond, selShowError :: CSSSel Class

-- Класс кнопок, переводящие программу из одного состояния в другое.
selBtnLink                           = CSSSel "btn-link"

selTxtAdvice                         = CSSSel "advice-text"

-- Отвечают за правильное отображения полей, располагающихся в одну строку.
selBoxFieldPair                      = CSSSel "field-pair-box"
selBoxFieldFirst                     = CSSSel "pair-box-first"
selBoxFieldSecond                    = CSSSel "pair-box-second"

selShowError                         = CSSSel "show-error"
