module Main.Login.Logic.General
  ( checkEmail ) where

--------------------------------------------------------------------------------
-- Функции, часто используемые в описании логики
-- входа пользователя в систему.
--------------------------------------------------------------------------------


-- | Проверяет, может ли существовать подобный email.
-- Чем тщательнее выполняется проверка, тем меньше работы
-- предстоит сделать серверу для выявления несуществующих
-- E-mail адресов.
checkEmail :: String -> Bool
checkEmail email
 |isBlank email          = False
 |(not . elem '@') email = False -- Нет '@' в строке
 |(length . flip filter email) (== '@') /= 1 = False -- Несколько '@'
 |(not . elem '.' . afterEmailSymbol) email  = False -- Нет '.' после '@'
 |last email == '.' = False -- Заканчивается на '.'
 |otherwise         = True
 where afterEmailSymbol email = (tail . snd . flip break email) (== '@')
       isBlank value          = length value == 0
