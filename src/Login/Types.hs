{-# LANGUAGE StrictData            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Login.Types where

import GHC.Generics          (Generic)
import Data.Typeable         (Typeable)
import Data.Data             (Data)
import Data.Binary           (Binary)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Proxy            (Proxy(..))
import Login.General         (checkEmail, checkPassw)
import Types.General         (LoginStage(..))
import Types.ServerAction    (ServerAction(..), ServerActionData(..), ServerActionResult
                             ,serverRequest)

-- ServerActionData --------------------------------------------------------------------------------

data SignInDatum = SignInDatum
  { signInEmail :: ByteString
  , signInPassw :: ByteString }
  deriving (Typeable, Generic)

data RecoveryDatum =
    EmailRecDatum    ByteString
  | KeyRecDatum      ByteString
  | NewPasswRecDatum ByteString
  deriving (Typeable, Generic)

instance Binary SignInDatum   where
instance Binary RecoveryDatum where

instance ServerActionData SignInDatum where
  validateData SignInDatum{..} = checkEmail signInEmail &&
                                 checkPassw (unpack signInPassw)

instance ServerActionData RecoveryDatum where
  validateData (EmailRecDatum    email) = checkEmail email
  validateData (KeyRecDatum          _) = undefined
  validateData (NewPasswRecDatum passw) = checkPassw (unpack passw)

-- ServerActionResult ------------------------------------------------------------------------------

data SignInResult =
    SignInAuthorized
  | SignInInvalidData
  deriving (Data)

data RecoveryResult =
    RecKeySent          -- Ключ отправлен на Email
  | RecInvalidEmail     -- Введен либо недопустимый, либо незарегистрированный Email.
  | RecInvalidKey       -- Ключ, введенный пользователем не совпадает с действительным.
  | RecValidKey
  | RecInvalidPassw     -- Недопустимый пароль.
                        -- NOTE: Пользователю необходимо вывести соотв. подсказку.
  | RecMismatchedPassws -- Значение поля 'Пароль' не совпадает со значение поля 'Повторите пароль'.
  | RecPasswChanged     -- Успешное завершение процедуры восстановления доступа.
  deriving (Data)

instance ServerActionResult SignInResult   where
instance ServerActionResult RecoveryResult where

-- ServerAction ------------------------------------------------------------------------------------

instance ServerAction SignInDatum SignInResult where
  runServerAction sock aData
   |validateData aData = serverRequest SignInStage sock aData (Proxy :: Proxy SignInResult)
   |otherwise = return SignInInvalidData

instance ServerAction RecoveryDatum RecoveryResult where
