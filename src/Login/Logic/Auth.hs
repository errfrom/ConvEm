{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Login.Logic.Auth
  (AuthData(..), AuthResult(..)
  ,authForm) where

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Events
import Control.Monad.Reader            (Reader, ask)
import Network.Socket                  (Socket)
import Data.ByteString.Char8           (ByteString, pack, unpack)
import Data.Word8                      (_space)
import Data.Monoid                     ((<>))
import Crypto.BCrypt                   (HashingPolicy(..))
import Templates.GenFlagAssociated     (deriveFlagAssociated)
import Types.General                   (FlagAssociated(..), Stage(Auth))
import Types.ServerAction       hiding (serverRequest)
import Login.Logic.General             (checkEmail, checkPassw)
import GUI                      hiding (on)
import qualified Data.ByteString    as BS     (singleton)
import qualified Types.ServerAction as Action (serverRequest)
import qualified Crypto.BCrypt      as BCrypt (hashPasswordUsingPolicy
                                              ,defaultHashAlgorithm)

-- Auth Data Desc --------------------------------------------------------------

type OpenPassword   = ByteString
type HashedPassword = ByteString

data AuthData =
  AuthData { authEmail :: ByteString
           , authPassw :: ByteString }

instance ServerActionData AuthData where
  asSingleBS AuthData{..} = authEmail <> BS.singleton _space <> authPassw
  validate   AuthData{..} = checkEmail authEmail && checkPassw (unpack authPassw)

-- Auth Result Desc ------------------------------------------------------------

data AuthResult = AuthCorrectData | AuthInvalidData
$(deriveFlagAssociated [ "AuthResult" ])

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

instance ServerAction AuthData AuthResult where
  runServerAction sock aData@AuthData{..}
   |validate aData = do
      hashedPassw <- hashPassw authPassw
      Action.serverRequest Auth sock (aData { authPassw = hashedPassw })
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
          let authData = AuthData email (pack passw)
          in do
            requestResult <- liftIO (runServerAction sock authData)
            case requestResult of
              AuthCorrectData -> return ()
              _               -> putError errBox errMsg
