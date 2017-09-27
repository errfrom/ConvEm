{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Login.Logic.Auth
  (AuthData(..), AuthResult(..)
  ,authForm) where

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Events
import Control.Monad.Reader            (Reader, ask)
import Network.Socket                  (Socket)
import GHC.Generics                    (Generic)
import Data.ByteString.Char8           (ByteString, pack, unpack)
import Data.Data                       (Data)
import Data.Binary                     (Binary(..), Get)
import Data.Default
import Data.Proxy                      (Proxy(..))
import Crypto.BCrypt                   (HashingPolicy(..))
import Types.General                   (Stage(Auth), LoginPrimaryData(..)
                                       ,defaultPut)
import Types.ServerAction       hiding (serverRequest)
import Login.Logic.General             (checkEmail, checkPassw)
import GUI
import qualified Data.Binary        as Bin    (get)
import qualified Types.ServerAction as Action (serverRequest)
import qualified Crypto.BCrypt      as BCrypt (hashPasswordUsingPolicy
                                              ,defaultHashAlgorithm)

-- Auth Data Desc --------------------------------------------------------------

type OpenPassword   = ByteString
type HashedPassword = ByteString

data AuthData a =
  AuthData { primaryData :: LoginPrimaryData a }
  deriving (Show, Foldable, Generic)

instance (Binary a) => Binary (AuthData a) where
  put = defaultPut
  get = do
    dataPrimary <- Bin.get :: Get (LoginPrimaryData a)
    return (AuthData dataPrimary)

instance ServerActionData (AuthData ByteString) where
  validateData AuthData{ primaryData = prim } = checkEmail (email prim)
                                             && checkPassw (unpack $ passw prim)

-- Auth Result Desc ------------------------------------------------------------

data AuthResult = AuthCorrectData | AuthInvalidData
  deriving (Data)

instance Default AuthResult where
  def = AuthInvalidData

instance Show AuthResult where
  show AuthCorrectData = "Sign In"
  show AuthInvalidData = "Please, enter the valid data and try again."

instance ServerActionResult AuthResult where

-- Action Desc -----------------------------------------------------------------

hashPassw :: OpenPassword -> IO HashedPassword
hashPassw passw = do
  (Just hashed) <- BCrypt.hashPasswordUsingPolicy hashingPolicy passw
  return hashed
  where hashCost = 11
        hashingPolicy = HashingPolicy hashCost BCrypt.defaultHashAlgorithm

instance ServerAction (AuthData ByteString) AuthResult where
  runServerAction sock aData
   |validateData aData =
      Action.serverRequest Auth sock aData (Proxy :: Proxy AuthResult)
   |otherwise = return AuthInvalidData

-- GUI Represantation ----------------------------------------------------------

authForm :: Reader Socket (UI ())
authForm = do
  sock <- ask
  return (worker sock)
  where worker sock =
          let enterKeyCode = 13
          in do
            body <- askWindow >>= getBody
            (inpEmail:inpPassw:_) <- mapM elCalled ["inp-email", "inp-passw"]
            (errBox:errMsg:_)     <- mapM elCalled ["action-header", "hdr-text"]
            on keyup body $ \keyCode ->
              if (keyCode == enterKeyCode)
                then do
                  email <- fmap pack (getValue inpEmail)
                  passw <- getValue inpPassw
                  makeAuthRequest sock email passw errBox errMsg
                else return ()
            mapM_ (\inp -> on focus inp $ \_ -> hideError errBox errMsg) [inpEmail, inpPassw]

        interactErrShow errBox errMsg funClass errText =
          let classErrShow = "as-error-container"
          in funClass errBox classErrShow >> element errMsg # set text errText >> return ()
        putError errBox errMsg =
          interactErrShow errBox errMsg addClass (show AuthInvalidData)
        hideError errBox errMsg =
          interactErrShow errBox errMsg removeClass (show AuthCorrectData)

        -- Пытается отправить запрос серверу на основе полученных данных.
        makeAuthRequest sock email passw errBox errMsg =
          let authData = AuthData (LoginPrimaryData email (pack passw))
          in do
            requestResult <- liftIO (runServerAction sock authData)
            case requestResult of
              AuthCorrectData -> return ()
              _               -> putError errBox errMsg
