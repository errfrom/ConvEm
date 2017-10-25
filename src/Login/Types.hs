{-# LANGUAGE StrictData            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TypeFamilies          #-}

module Login.Types where

import GHC.Generics          (Generic)
import Data.Typeable         (Typeable)
import Data.Data             (Data)
import Data.Binary           (Binary)
import Data.ByteString.Char8 (ByteString, unpack)
import Login.General         (checkEmail, checkPassw)
import Types.General         (LoginStage(..))
import Types.ServerAction    (ServerAction(..), ServerActionData(..), serverRequest)

-- ServerActionData --------------------------------------------------------------------------------

data SignInDatum = SignInDatum
  { signInEmail :: ByteString
  , signInPassw :: ByteString }
  deriving (Data, Typeable, Generic)

data RecoveryDatum =
    EmailRecDatum    ByteString
  | KeyRecDatum      ByteString
  | NewPasswRecDatum ByteString
  deriving (Data, Typeable, Generic)

instance Binary SignInDatum   where
instance Binary RecoveryDatum where

instance ServerActionData SignInDatum where
  validateData SignInDatum{..} = checkEmail signInEmail &&
                                 checkPassw (unpack signInPassw)

instance ServerActionData RecoveryDatum where
  validateData (EmailRecDatum    email) = checkEmail email
  validateData (KeyRecDatum          _) = undefined
  validateData (NewPasswRecDatum passw) = checkPassw (unpack passw)

-- ServerAction ------------------------------------------------------------------------------

instance ServerAction SignInDatum where
  data ServerActionResult SignInDatum = SignInAuthorized | SignInInvalidData
    deriving (Data)

  runServerAction sock aData
   |validateData aData = serverRequest SignInStage sock aData
   |otherwise = return SignInInvalidData

instance ServerAction RecoveryDatum where
  data ServerActionResult RecoveryDatum =
      RecKeySent          -- Ключ отправлен на Email
    | RecInvalidEmail     -- Введен либо недопустимый, либо незарегистрированный Email.
    | RecInvalidKey       -- Ключ, введенный пользователем не совпадает с действительным.
    | RecValidKey
    | RecInvalidPassw     -- Недопустимый пароль.
                          -- NOTE: Пользователю необходимо вывести соотв. подсказку.
    | RecMismatchedPassws -- Значение поля 'Пароль' не совпадает со значение поля 'Повторите пароль'.
    | RecPasswChanged     -- Успешное завершение процедуры восстановления доступа.
    deriving (Data)

  runServerAction = undefined
