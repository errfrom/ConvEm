{-# OPTIONS_GHC -fno-warn-orphans             #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Main.Login.Auth where

import Graphics.UI.Threepenny.Core             hiding    (value)
import qualified Graphics.UI.Threepenny.Events as Events (focus)
import qualified Data.ByteString.Char8         as BS     (pack)
import qualified Control.Monad                 as Monad  (void)
import Types.Hierarchy
import Types.Results
import Types.Data
import Main.Login.GUI
import qualified Main.Login.Logic as Logic (checkEmail
                                           ,askServerResp)
import qualified Utils                     (getValue, getElemById
                                           ,removeClass)

--------------------------------------------------------------------------------

instance Callable Auth AuthResult where
  call Auth sock (email:passw:_) =
    let loginData = AuthData (BS.pack email) (BS.pack passw)
    in  (Logic.askServerResp sock loginData)

--------------------------------------------------------------------------------

instance Checkable Auth AuthResult where
  check _ (email:passw:_) =
    case (worker email passw) of
      []       -> Nothing
      mistakes -> Just (InvalidValues mistakes)
    where isBlank value = length value == 0
          checkPassw    = (not . isBlank)
          checkValue toCheck funCheck res
           |funCheck toCheck = []
           |otherwise        = res : []
          worker email' passw' =
            (checkValue email' Logic.checkEmail InEmailField) ++
            (checkValue passw' checkPassw InPasswordField)

--------------------------------------------------------------------------------

instance Manageable Auth AuthResult where
  handle _ sock = do
    (invalidInpBox:inpEmail:inpPassw:_) <- mapM Utils.getElemById
                                             [ "invalid-input-text"
                                             , "inp-email"
                                             , "inp-passw" ]
    fieldVals <- mapM Utils.getValue [ inpEmail, inpPassw ]
    let onFocusRemoveErrClass = onFocusRemoveErrClass' invalidInpBox
        showErrText           = (Monad.void . showErrText' invalidInpBox)
        checkResult           = check Auth fieldVals
    mapM_ onFocusRemoveErrClass [ inpEmail, inpPassw ]
    case checkResult of
      Nothing -> do
        callResult <- liftIO (call Auth sock fieldVals)
        case callResult of
          CorrectPassword     -> return () -- TODO: Переход к основному окну.
          IncorrectPassword   -> showErrText "Введенный пароль неверен."
          ANonexistentAccount -> showErrText "Аккаунта с таким Email не существует."
          BlockedAccount      -> showErrText "Аккаунт заблокирован."
      Just (InvalidValues mistake) ->
        case mistake of
          [ InEmailField ] -> do
            showErrText "Введите корректный E-mail адрес."
            setErrClass inpEmail
          [ InPasswordField ] -> do
            showErrText "Введите корректный пароль."
            setErrClass inpPassw
          [ InEmailField, InPasswordField ] -> do
            showErrText "Введите корректные данные."
            mapM_ setErrClass [ inpEmail, inpPassw ]
    where errClass                = "with-error"
          setErrClass el          = Monad.void $ (return el) #. errClass
          showErrText' inpBox msg = (return inpBox) #. errClass
                                                    #  set text msg
          onFocusRemoveErrClass' inpBox el =
            on Events.focus el $ \_ ->
              mapM_ (flip Utils.removeClass errClass) [ el, inpBox ]

--------------------------------------------------------------------------------

instance Form Auth AuthResult where
  form _ sock = do
    build "login-form"
      [ wrap [ add (LblHeader "Авторизация")
             , add (LblDesc   "Введите ваш E-mail и пароль для продолжения работы.") ]
      , wrap [ add (InpSimple "E-mail"  ) `as` "inp-email" ]
      , wrap [ add (InpPassword "Пароль") `as` "inp-passw" ]
      , add LblInvalid
      , wrap [ add (BtnImportant "Вперед") `bind` (handle Auth sock)
             , additional [ add $ BtnLink "Регистрация"
                          , add $ BtnLink "Забыли пароль?" ]]]
