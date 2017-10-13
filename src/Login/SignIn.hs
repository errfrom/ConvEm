{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Login.SignIn
  ( SignInData(..), SignInResult(..), logInSetup ) where

import Control.Monad.Trans.Reader          (ReaderT(..), ask)
import Control.Monad.IO.Class              (liftIO)
import Control.Monad                       (void)
import Control.Concurrent.MVar             (MVar, putMVar)
import Network.Socket                      (Socket)
import GHC.Generics                        (Generic)
import Data.ByteString.Char8               (ByteString, pack, unpack)
import Data.Data                           (Data)
import Data.Binary                         (Binary(..), Get)
import Data.Default                        (Default, def)
import Data.Proxy                          (Proxy(..))
import Crypto.BCrypt                       (HashingPolicy(..))
import Types.General                       (LoginStage(..), LoginPrimaryData(..), defaultPut)
import Login.General                       (checkEmail, checkPassw)
import Types.ServerAction hiding           (serverRequest)
import Graphics.UI.Gtk                     (Window)
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Graphics.Form
import Graphics.Data.Forms
import Graphics.Data.Selectors
import Graphics.General
import qualified Data.Binary        as Bin    (get)
import qualified Types.ServerAction as Action (serverRequest)
import qualified Crypto.BCrypt      as BCrypt (hashPasswordUsingPolicy
                                              ,defaultHashAlgorithm)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document as Doc

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
   |validateData aData =
      Action.serverRequest SignInStage sock aData (Proxy :: Proxy SignInResult)
   |otherwise = return SignInInvalidData

-- GUI Represantation ----------------------------------------------------------

logInSetup :: (DocumentClass doc) => Window -> Socket -> MVar Bool -> ReaderT doc IO ()
logInSetup win sock mvarAuthorized =
  let keyEnter = "Return"
  in do
    uiHeight <- buildForm formSignIn
    basicFormSetup uiHeight
    setSwitch SignUpStage (Just RecoveryStageEmail)
    doc <- ask
    (void . liftIO) $ onPress win keyEnter $ do
      (Just email:Just passw:_) <- mapM (getValue doc) [ selInpEmail, selInpPassw ]
      let authData = SignInData $ LoginPrimaryData (pack email) (pack passw)
      actionResult <- runServerAction sock authData
      case actionResult of
        SignInCorrectData -> putMVar mvarAuthorized True
        SignInInvalidData -> runReaderT (notifyError uiHeight errInvalidData) doc