{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Login.SignIn
  ( SignInData(..), SignInResult(..) ) where

import GHC.Generics                           (Generic)
import Data.ByteString.Char8                  (ByteString, unpack)
import Data.Data                              (Data)
import Data.Binary                            (Binary(..), Get)
import Data.Default                           (Default, def)
import Data.Proxy                             (Proxy(..))
import Crypto.BCrypt                          (HashingPolicy(..))
import Types.General                          (LoginStage(..), LoginPrimaryData(..), defaultPut)
import Login.General                          (checkEmail, checkPassw)
import Types.ServerAction hiding              (serverRequest)
import qualified Data.Binary        as Bin    (get)
import qualified Types.ServerAction as Action (serverRequest)
import qualified Crypto.BCrypt      as BCrypt (hashPasswordUsingPolicy
                                              ,defaultHashAlgorithm)

-- Auth Data Desc --------------------------------------------------------------

type OpenPassword   = ByteString
type HashedPassword = ByteString

data SignInData a =
  SignInData { primaryData :: LoginPrimaryData a }
  deriving (Show, Foldable, Generic)

instance (Binary a) => Binary (SignInData a) where
  put = defaultPut
  get = do
    dataPrimary <- Bin.get :: Get (LoginPrimaryData a)
    return (SignInData dataPrimary)

instance ServerActionData (SignInData ByteString) where
  validateData SignInData{ primaryData = prim } =
    checkEmail (email prim) && checkPassw (unpack $ passw prim)

-- Auth Result Desc ------------------------------------------------------------

data SignInResult = SignInCorrectData | SignInInvalidData
  deriving (Data)

instance Default SignInResult where
  def = SignInInvalidData

instance ServerActionResult SignInResult where

-- Action Desc -----------------------------------------------------------------

hashPassw :: OpenPassword -> IO HashedPassword -- FIXME
hashPassw passw = do
  (Just hashed) <- BCrypt.hashPasswordUsingPolicy hashingPolicy passw
  return hashed
  where hashCost = 11
        hashingPolicy = HashingPolicy hashCost BCrypt.defaultHashAlgorithm

instance ServerAction (SignInData ByteString) SignInResult where
  runServerAction sock aData
   |validateData aData = Action.serverRequest SignInStage sock aData (Proxy :: Proxy SignInResult)
   |otherwise = return SignInInvalidData
