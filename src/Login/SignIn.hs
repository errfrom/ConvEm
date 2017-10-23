{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}

module Login.SignIn
  ( SignInData(..), SignInResult(..)
  , SignInDataBS ) where

import GHC.Generics                           (Generic)
import Data.ByteString.Char8                  (ByteString, unpack)
import Data.Data                              (Data)
import Data.Typeable                          (Typeable)
import Data.Binary                            (Binary(..), Get)
import Data.Default                           (Default, def)
import Data.Proxy                             (Proxy(..))
import Crypto.BCrypt                          (HashingPolicy(..))
import Types.General                          (LoginStage(..), defaultPut)
import Login.General                          (checkEmail, checkPassw)
import Types.ServerAction hiding              (serverRequest)
import qualified Data.Binary        as Bin    (get)
import qualified Types.ServerAction as Action (serverRequest)
import qualified Crypto.BCrypt      as BCrypt (hashPasswordUsingPolicy
                                              ,defaultHashAlgorithm)

-- Auth Data Desc --------------------------------------------------------------

type OpenPassword   = ByteString
type HashedPassword = ByteString
type SignInDataBS   = SignInData ByteString

data SignInData a =
  SignInData { signInEmail :: a
             , signInPassw :: a }
  deriving (Show, Typeable, Foldable, Generic)

instance (Binary a) => Binary (SignInData a) where
  put = defaultPut
  get = (Bin.get :: Get [a]) >>= \(e:p:_) -> return (SignInData e p)

instance ServerActionData (SignInData ByteString) where
  validateData SignInData{..} = checkEmail signInEmail && checkPassw (unpack signInPassw)

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

instance ServerAction SignInDataBS SignInResult where
  runServerAction sock aData
   |validateData aData = Action.serverRequest SignInStage sock aData (Proxy :: Proxy SignInResult)
   |otherwise = return SignInInvalidData
