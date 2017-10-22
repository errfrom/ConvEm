{-# LANGUAGE OverloadedStrings #-}

module Login.Logic ( logInSetup ) where

import Control.Monad                       (void)
import Control.Monad.IO.Class              (liftIO)
import Control.Monad.Trans.Reader          (ReaderT(..), ask)
import Data.Text                           (Text)
import Data.ByteString.Char8               (pack)
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Login.SignIn 
import Graphics.Data.Selectors
import Graphics.Data.Forms
import Graphics.Form
import Graphics.General
import Types.General
import Types.ServerAction                  (runServerAction)
import qualified Graphics.UI.Gtk         as Gtk  (postGUIAsync)
import qualified Control.Concurrent.MVar as MVar (putMVar)
import qualified Control.Concurrent      as Conc (forkIO)

keyEnter :: Text
keyEnter = "Return"

initForm :: (DocumentClass doc) => doc -> UIFormBuilder
                                -> LoginStage -> Maybe LoginStage
                                -> AppM doc UIHeight
initForm doc formBuilder fstStage mSndStage = do
  uiHeight <- (liftIO . flip runReaderT doc) $ do
    uiHeight' <- buildForm formBuilder
    hideError uiHeight' -- NOTE
    basicFormSetup uiHeight'
    return uiHeight'
  setSwitch fstStage mSndStage
  return uiHeight

setSwitch :: (DocumentClass doc) => LoginStage -> Maybe LoginStage -> AppM doc ()
setSwitch fstStage mSndStage = do
  worker fstStage selSwitchFst
  case mSndStage of Nothing       -> return ()
                    Just sndStage -> worker sndStage selSwitchSnd
  where worker stage sel = do
          appRef    <- ask
          doc       <- getAppM appDoc
          (liftIO . flip runReaderT doc . operateElemById sel) $ \btnSwitch ->
            onClick btnSwitch $ flip runReaderT appRef (setUpStage stage)

        setUpStage SignInStage = logInSetup -- TODO

logInSetup :: (DocumentClass doc) => AppM doc ()
logInSetup = void $ do
  app <- askApp
  let doc = appDoc app
  uiHeight <- initForm doc formSignIn RecoveryStageEmail (Just SignUpStage)
  liftIO $ onPress (appWin app) keyEnter $ do
    (Just email:Just passw:_) <- mapM (getValue doc) [ selInpEmail, selInpPassw ]
    let authData = SignInData $ LoginPrimaryData (pack email) (pack passw)
    (void . Conc.forkIO) $ do
      actionResult <- withNoConnHandling doc $ runServerAction (appSock app) authData
      case actionResult of
        SignInCorrectData -> MVar.putMVar (appAuthorized app) True
        SignInInvalidData -> Gtk.postGUIAsync $
          runReaderT (notifyError uiHeight errInvalidData) doc
