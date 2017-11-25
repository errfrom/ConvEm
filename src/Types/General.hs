{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Types.General
  ( AppM, App(..), modifyAppM, getAppM, askApp
  , module Frames, askForElement, withFrame, releaseFrame
  , translateInterface ) where

import Control.Concurrent                  (MVar)
import Control.Monad.Trans.Reader          (ReaderT, ask)
import Control.Monad.IO.Class              (liftIO)
import Data.Typeable                       (Typeable, Proxy(..), typeRep, typeOf, typeRepFingerprint)
import Data.Foldable                       (toList)
import Data.Default                        (Default, def)
import Data.Data                           (Data)
import Data.Dynamic                        (toDyn, dynApp, fromDynamic)
import Data.Binary                         (Binary(..), Put)
import Data.IORef                          (IORef, modifyIORef, readIORef)
import Data.Set                            (Set)
import Network.Socket                      (Socket)
import Graphics.UI.Gtk                     (Window)
import Graphics.UI.Gtk.WebKit.WebView      (WebView)
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.Element  (Element)
import Types.Frames              as Frames
import qualified Data.Binary     as Bin    (putList)
import qualified Data.Set        as S      (toList, fromList, insert, delete, notMember)

modifyAppM :: (DocumentClass d) => (App d -> App d) -> AppM d ()
modifyAppM f = ask >>= \appRef -> liftIO $ modifyIORef appRef f

getAppM :: (DocumentClass d) => (App d -> a) -> AppM d a
getAppM f = ask >>= \appRef -> liftIO $ readIORef appRef >>= return . f

askApp :: (DocumentClass d) => AppM d (App d)
askApp = ask >>= liftIO . readIORef

-- Managing frames inside the AppM monad -----------------------------------------------------------

-- NOTE: safe.
-- Looks for a specific element among existing frames.
askForElement :: forall frameData d. (DocumentClass d, FrameClass frameData)
              => (FrameElements' frameData -> Element)
              -> AppM d (Maybe Element)
askForElement f = getAppM appFrames >>= \frameWrappers ->
  let trFrame       = typeRep (Proxy :: Proxy (FrameElements' frameData))
  in return $
    case (getMatchedFrame trFrame $ S.toList frameWrappers) of
      -- This function won't be applied because there is not enough info about types.
      -- So we apply it dynamically. (Runtime errors are impossible)
      Just (AnyFrame frameElements _) -> fromDynamic (toDyn f `dynApp` toDyn frameElements) :: Maybe Element
      Nothing                            -> Nothing
  where getMatchedFrame _ [] = Nothing
        getMatchedFrame trFrame (frameWrapper@(AnyFrame frameElements _):xs)
         | eqTypeRep trFrame (typeOf frameElements) = Just frameWrapper
         | otherwise                                = getMatchedFrame trFrame xs
        eqTypeRep a b = typeRepFingerprint a == typeRepFingerprint b

updateFrames :: (DocumentClass doc) => (Set AnyFrame -> Set AnyFrame) -> AppM doc ()
updateFrames f = modifyAppM $ \app -> let oldFrames = appFrames app
                                      in app { appFrames = f oldFrames }

withFrame :: (DocumentClass doc, FrameClass frameData)
          => frameData
          -> ((FrameElements' frameData, ReleaseFunction, AnyFrame) -> AppM doc a)
          -> AppM doc a
withFrame builder' behavior = do
  lang <- getAppM appLanguage
  let builder = translate lang builder'
  doc <- getAppM appDoc
  (releaser, frame) <- liftIO (initFrame doc builder)
  let frameWrapper = AnyFrame frame builder
  updateFrames (S.insert frameWrapper)
  behavior (frame, releaser, frameWrapper)

releaseFrame :: (DocumentClass doc) => ReleaseFunction -> AnyFrame -> AppM doc ()
releaseFrame releaser frameWrapper = getAppM appFrames >>= worker
  where worker frames
         | S.notMember frameWrapper frames = return ()
         | otherwise                       = do updateFrames (S.delete frameWrapper)
                                                liftIO (runReleaser releaser)

translateInterface :: (DocumentClass doc) => Language -> AppM doc ()
translateInterface lang = do doc <- getAppM appDoc
                             frames <- getAppM appFrames
                             framesLocalized <- liftIO $ mapM (translateOne doc) (S.toList frames)
                             modifyAppM $ \app -> app { appLanguage = lang
                                                      , appFrames = S.fromList framesLocalized }
  where translateOne :: (DocumentClass doc) => doc -> AnyFrame -> IO AnyFrame
        translateOne doc (AnyFrame frameElements frameData) =
          let newFrameData = translate lang frameData
          in updateFrame doc frameElements newFrameData >> return (AnyFrame frameElements newFrameData)

-- App ---------------------------------------------------------------------------------------------

type AppM doc a = ReaderT (IORef (App doc)) IO a

data App doc = App
  { appDoc        :: doc
  , appWin        :: Window
  , appWV         :: WebView
  , appSock       :: Socket
  , appAuthorized :: MVar Bool
  , appLanguage   :: Language
  , appFrames     :: Set AnyFrame }
