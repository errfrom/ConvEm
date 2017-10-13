{-# LANGUAGE OverloadedStrings #-}

module Login.General
  (checkEmail, checkPassw) where

import Data.ByteString.Char8     (ByteString, unpack)
import Text.Regex                (mkRegex, matchRegex)
import qualified Data.Maybe as M (isJust)

-- Проверяет, может ли существовать такой Email.
checkEmail :: ByteString -> Bool
checkEmail email =
  let regexEmail = mkRegex $
        "^[a-z0-9][a-z0-9_.-]+[a-z0-9]@[a-z0-9-]+[.]([a-z]{2,6}[.])?[a-z]{2,6}$"
  in (M.isJust . matchRegex regexEmail . unpack) email

checkPassw :: String -> Bool -- TODO:
checkPassw passw = (length passw) >= 9
