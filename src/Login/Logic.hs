{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Login.Logic ( signInSetup ) where

import Control.Monad                       (void)
import Control.Monad.IO.Class              (liftIO)
import Control.Monad.Trans.Reader          (ReaderT(..), ask)
import Data.Dynamic                        (Dynamic, Typeable)
import Data.Proxy                          (Proxy(..))
import Data.ByteString.Char8               (pack)
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Login.Types
import Graphics.Data.Selectors
import Graphics.Data.Forms
import Graphics.Form
import Graphics.General
import Types.General
import Types.ServerAction
import qualified Data.Dynamic                                as Dyn  (dynApp, toDyn, fromDynamic)
import qualified Data.Maybe                                  as M    (catMaybes)
import qualified Graphics.UI.Gtk                             as Gtk  (postGUIAsync)
import qualified Control.Concurrent.MVar                     as MVar (putMVar)
import qualified Control.Concurrent                          as Conc (forkIO)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement as Inp  (getValue)

-- Обобщенно формирует необходимые данные и выполняет запрос к серверу.
-- На основе введеных пользователем значениях формируются данные,
-- а после динамически создается соответствующий блок данных.
withRunServerAction :: forall proxy data'. forall doc res.
                     ( DocumentClass doc, Typeable data', ServerAction data' res)
                    => Dynamic -> proxy data' -> (res -> IO ()) -> AppM doc ()
withRunServerAction constrDynamic _ behavior = do
  app <- askApp
  let doc = appDoc app
  (onPressEnter . void) $ do
    inpVals <- getInputs doc >>= mapM Inp.getValue >>= return . M.catMaybes
    let inpValsDynamic    = map (Dyn.toDyn . pack) inpVals
        dataDynamic       = apply constrDynamic inpValsDynamic
        (Just actionData) = Dyn.fromDynamic dataDynamic :: Maybe data'
    Conc.forkIO $ do
      actionResult <- withNoConnHandling doc $ runServerAction (appSock app) actionData
      behavior actionResult
  where onPressEnter :: (DocumentClass doc) => IO () -> AppM doc ()
        onPressEnter action = void $ do
          win <- getAppM appWin
          liftIO (onPress win "Return" action)

        apply :: Dynamic -> [Dynamic] -> Dynamic
        apply t []     = t
        apply t (x:xs) = apply (Dyn.dynApp t x) xs

-- Устанавливает свойства формы, не зависящие от введенных пользователем данных.
initForm :: (DocumentClass doc) => UIFormBuilder
                                -> LoginStage -> Maybe LoginStage
                                -> AppM doc UIHeight
initForm formBuilder fstStage mSndStage = do
    doc <- getAppM appDoc
    uiHeight <- (liftIO . flip runReaderT doc) $ do
      uiHeight' <- buildForm formBuilder
      hideError uiHeight'
      basicFormSetup uiHeight'
      return uiHeight'
    setSwitch fstStage mSndStage
    return uiHeight
    where -- Связывает кнопки навигации с переключением на определенные этапы.
          setSwitch :: (DocumentClass doc) => LoginStage -> Maybe LoginStage -> AppM doc ()
          setSwitch fstStage mSndStage = do
            worker fstStage selSwitchFst
            case mSndStage of Nothing       -> return ()
                              Just sndStage -> worker sndStage selSwitchSnd
            where worker stage sel = do
                    appRef <- ask
                    doc    <- getAppM appDoc
                    (liftIO . flip runReaderT doc . operateElemById sel) $ \btnSwitch ->
                      onClick btnSwitch $ flip runReaderT appRef (setUpStage stage)

setUpStage :: (DocumentClass doc) => LoginStage -> AppM doc ()
setUpStage stage = case stage of SignInStage        -> signInSetup
                                 RecoveryStageEmail -> recEmailSetup

signInSetup, recEmailSetup :: (DocumentClass doc) => AppM doc ()

signInSetup = do
  app <- askApp
  let doc = appDoc app
  uiHeight <- initForm formSignIn RecoveryStageEmail (Just SignUpStage)
  withRunServerAction (Dyn.toDyn SignInDatum) (Proxy :: Proxy SignInDatum) $ \actionResult ->
    case actionResult of
      SignInAuthorized  -> MVar.putMVar (appAuthorized app) True
      SignInInvalidData -> Gtk.postGUIAsync $ runReaderT (notifyError uiHeight errInvalidData) doc

recEmailSetup = do
  appRef   <- ask
  doc      <- getAppM appDoc
  uiHeight <- initForm formEmailRecovery SignInStage (Just SignUpStage)
  withRunServerAction (Dyn.toDyn EmailRecDatum) (Proxy :: Proxy RecoveryDatum) $ \actionResult ->
    Gtk.postGUIAsync $ case actionResult of
                         RecKeySent      -> flip runReaderT appRef (setUpStage RecoveryStageKey)
                         RecInvalidEmail -> runReaderT (notifyError uiHeight errInvalidEmail) doc
