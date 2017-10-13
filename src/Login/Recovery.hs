{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}

module Login.Recovery where

import GHC.Generics                     (Generic)
import Data.Default                     (Default(..))
import Data.Binary                      (Binary(..), Get)
import Data.Data                        (Data)
import Data.Proxy                       (Proxy(..))
import Data.String                      (IsString)
import Data.ByteString.Char8            (ByteString, unpack)
import Login.General              (checkEmail, checkPassw)
import Types.ServerAction
import qualified Data.Foldable  as Fold (toList)
import qualified Data.Binary    as Bin  (putList, get)

-- Recovery Data Desc ----------------------------------------------------------

data RecoveryData  a =
  EmailRecovery    a
 |KeyRecovery      a
 |NewPasswRecovery a
  deriving (Show, Data, Functor, Foldable, Generic)

instance (IsString a) => Default (RecoveryData a) where
  def = EmailRecovery ""

instance Binary (RecoveryData ByteString) where
  put dataRecovery =
    let flag = constrAsFlag dataRecovery
    in  Bin.putList (flag : Fold.toList dataRecovery)
  get = do
    (flag:dataRecovery:_) <- Bin.get :: Get [ByteString]
    let constr = flagAsConstr flag (Proxy :: Proxy (RecoveryData ByteString))
    return $ fmap (\_ -> dataRecovery) constr

instance ServerActionData (RecoveryData ByteString) where
  validateData (EmailRecovery    email) = checkEmail email
  validateData (KeyRecovery      key)   = undefined
  validateData (NewPasswRecovery passw) = checkPassw (unpack passw)

--------------------------------------------------------------------------------
