{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main.Login.Decls where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Events as Events
import Control.Monad            (void)
import Network.Socket           (Socket)
import Types.General            (FlagAssociated(..))
import Types.Results            (MistakeIn(..), Result(..), AuthResult(..)
                                ,RecoveryResult(..))
import Types.Data               (UserData(..))
import Data.ByteString.Char8    (pack)
import Utils                    (getValue, getElemById, removeClass)
import Main.Login.Logic.General (checkEmail, askServerResp)
import Main.Login.GUI.General

type Value = String
data Auth  = Auth
class Stage t where
instance Stage Auth

class (Stage s, FlagAssociated r, Result r) => Callable s r | s -> r where
  -- | Вызывается, когда уже известно, что пользователем введены
  -- корректные данные, проверенные функцией check.
  -- Последний уровень обработки данных на клиентской стороне.
  call :: s -> Socket -> [Value] -> IO r


class (Callable s r) => Checkable s r | s -> r where
  -- | Проверяет, совершил ли пользователь ошибки при вводе.
  -- Например, оповещает, если пользователь ввел специальные символы в поле
  -- ввода имени.
  check :: s -> [Value] -> Maybe r

class (Checkable s r) => Manageable s r | s -> r where
  -- | Функция, делигирующая работу функциям check и call и,
  -- впоследствии выполняющая функцию отображения результатов
  -- пользователю посредством графического интерефейса.
  handle :: s -> Socket -> UI ()

class (Manageable s r) => Form s r | s -> r where
  form :: s -> Socket -> UI Element

instance Callable Auth AuthResult where
  call Auth sock (email:passw:_) =
    let loginData = AuthData (pack email) (pack passw)
    in  (askServerResp sock loginData)

instance Checkable Auth AuthResult where
  check Auth (email:password:_)
   |checkPassword password !&& checkEmail email = Just (InvalidValues InBothFields)
   |(not . checkEmail) email       = Just (InvalidValues InEmailField)
   |(not . checkPassword) password = Just (InvalidValues InPasswordField)
   |otherwise                      = Nothing
    where (!&&) a b     = (not a) && (not b)
          isBlank value = length value == 0
          checkPassword = (not . isBlank)

instance Manageable Auth AuthResult where
  handle Auth sock = do
    invalidInpBox <- getElemById "invalid-input-text"
    inpEmail      <- getElemById "inp-email"
    inpPassw      <- getElemById "inp-passw"
    email         <- getValue inpEmail
    passw         <- getValue inpPassw
    let onFocusRemoveErrClass = onFocusRemoveErrClass' invalidInpBox
        showErrText           = showErrText' invalidInpBox
        checkResult           = check Auth [ email, passw ]
    case checkResult of
      Nothing -> do
        callResult <- liftIO (call Auth sock [ email, passw ])
        case callResult of
          CorrectPassword     -> return () -- TODO: Переход к основному окну.
          IncorrectPassword   -> void $ showErrText "Введенный пароль неверен."
          ANonexistentAccount -> void $ showErrText "Аккаунта с таким Email не существует."
          BlockedAccount      -> void $ showErrText "Аккаунт заблокирован."
      Just (InvalidValues mistake) ->
        case mistake of
          InEmailField -> do
            showErrText "Введите корректный E-mail адрес."
            void (setErrClass inpEmail)
          InPasswordField -> do
            showErrText "Введите корректный пароль."
            void (setErrClass inpPassw)
          InBothFields -> do
            showErrText "Введите корректные данные."
            mapM_ setErrClass [ inpEmail, inpPassw ]
    return ()
    where errClass                = "with-error"
          setErrClass el          = (return el) # set (attr "class") errClass
          showErrText' inpBox msg = (return inpBox) # set (attr "class") "with-error"
                                                    # set text msg
          onFocusRemoveErrClass' inpBox el =
            on Events.focus el $ \_ ->
              mapM_ (flip removeClass errClass) [ el, inpBox ]

instance Form Auth AuthResult where
  form Auth sock = do
    window <- askWindow
    build "login-form"
      [ wrap [ add (LblHeader "Авторизация")
             , add (LblDesc   "Введите ваш E-mail и пароль для продолжения работы.") ]
      , wrap [ add (InpSimple "E-mail"  ) `as` "inp-email" ]
      , wrap [ add (InpPassword "Пароль") `as` "inp-passw" ]
      , add LblInvalid
      , wrap [ add (BtnImportant "Вперед") `bind` (handle Auth sock)
             , additional [ add $ BtnLink "Регистрация"
                          , add $ BtnLink "Забыли пароль?" ]]]
