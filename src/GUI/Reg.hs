module GUI.Reg where

import qualified Data.Char as Ch  (isDigit, isUpper)

type Advice   = String
type Password = String

checkPassword :: Password -> [Advice]
checkPassword password = worker password [checkLength, checkNumbers, checkUpper]
 where worker :: Password -> [(Password -> Maybe Advice)] -> [Advice]
       worker _ [] = []
       worker password (fun:funcs) =
         case (fun password) of
           Just advice -> advice : (worker password funcs)
           Nothing     -> [] ++ (worker password funcs)

       checkLength password
        |length password >= 9 = Nothing
        |otherwise            = Just "быть не короче 9 символов."
       checkNumbers password
        |length (filter Ch.isDigit password) >= 2 = Nothing
        |otherwise                                = Just "содержать не менее 2 цифр."
       checkUpper password
        |length (filter Ch.isUpper password) >= 1 = Nothing
        |otherwise                                = Just "содержать заглваную букву."
