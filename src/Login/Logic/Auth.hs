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
import Data.ByteString.Char8           (ByteString, pack)
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

type OpenPassword   = ByteString
type HashedPassword = ByteString

-- Auth Data Desc --------------------------------------------------------------

data AuthData a =
  AuthData { authEmail :: ByteString
           , authPassw :: a }

instance ServerActionData (AuthData HashedPassword) where
  asSingleBS AuthData{..} = authEmail <> BS.singleton _space <> authPassw

-- Auth Result Desc ------------------------------------------------------------

data AuthResult =
  AuthCorrectPassword
 |AuthIncorrectPassword
 |AuthNonexistentAccount
 |AuthBlockedAccount
$(deriveFlagAssociated [ "AuthResult" ])

instance Show AuthResult where
  show AuthCorrectPassword    = "AuthCorrectPassword"
  show AuthIncorrectPassword  = "The entered password is incorrect."
  show AuthNonexistentAccount = "This account does not exist."
  show AuthBlockedAccount     = "This account has been blocked."

instance ServerActionResult AuthResult where

-- Action Desc -----------------------------------------------------------------

hashPassw :: OpenPassword -> IO HashedPassword
hashPassw passw = do
  (Just hashed) <- BCrypt.hashPasswordUsingPolicy hashingPolicy passw
  return hashed
  where hashCost = 11
        hashingPolicy = HashingPolicy hashCost BCrypt.defaultHashAlgorithm

instance ServerAction (AuthData OpenPassword) AuthResult where
  runServerAction sock aData@AuthData{..} = do
    hashedPassw <- hashPassw authPassw
    Action.serverRequest Auth sock (aData { authPassw = hashedPassw })

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
            on keyup body $ \keyCode ->
              if (keyCode == enterKeyCode)
                then authRequest sock inpEmail inpPassw
                else return ()

        classMistakeShow = "show-mistake"
        putMistake mistakeBox mistakeMsg mistake = do
          addClass mistakeBox classMistakeShow
          element mistakeMsg # set text mistake
          return ()
        hideMistake mistakeBox = removeClass mistakeBox classMistakeShow

        authRequest sock inpEmail inpPassw = -- TODO : refactor
          let classMistakeShow = "show-mistake"
          in do
            (mistakeBox:mistakeMsg:_) <- mapM elCalled ["mistake-box", "mistake-msg"]
            email      <- fmap pack (getValue inpEmail)
            passw      <- getValue inpPassw
            if (checkEmail email && checkPassw passw)
              then do
                let authData = AuthData email (pack passw)
                requestResult <- liftIO (runServerAction sock authData)
                case requestResult of
                  AuthCorrectPassword -> hideMistake mistakeBox
                  _ -> putMistake mistakeBox mistakeMsg (show requestResult)
              else putMistake mistakeBox mistakeMsg "Please, input the correct data."
