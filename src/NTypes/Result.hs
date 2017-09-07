module NTypes.Result
  (IsResult, Result(..)
  ,UserInputMistake(..)
  ,AuthResult(..)) where

--------------------------------------------------------------------------------
-- Наборы данных, описывающие результаты какого-либо события.
--------------------------------------------------------------------------------

-- Разновидность предварительного результата.
-- В каком поле ввода пользователь допустил ошибку.
data UserInputMistake =
  InEmailField
 |InPasswField

class IsResult t where
instance IsResult AuthResult

-- Обобщенная модель результата какого-либо действия.
data Result a b =
  PreResult a
 |ServerResponse b

data AuthResult =
  AuthInvalidInput [UserInputMistake]
 |AuthCorrectPassword
 |AuthIncorrectPassword
 |AuthNonexistentAccount
 |AuthBlockedAccount
