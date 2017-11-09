{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Types.General
  ( LoginStage(..)
  , defaultPut
  , AppM, App(..), modifyAppM, getAppM, askApp
  , module Frames, askForElement, withFrame ) where

--------------------------------------------------------------------------------
-- Объявления, часто использующиеся в других модулях.
--------------------------------------------------------------------------------

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
import Data.Text                           (Text)
import Network.Socket                      (Socket)
import Graphics.UI.Gtk                     (Window)
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.Element  (Element)
import System.Glib.UTFString               (GlibString)
import Types.Frames
import qualified Types.Frames    as Frames hiding (AnyFrame(..))
import qualified Data.Binary     as Bin    (putList)
import qualified Data.Set        as S      (toList, insert, delete)

modifyAppM :: (DocumentClass d) => (App d -> App d) -> AppM d ()
modifyAppM f = ask >>= \appRef -> liftIO $ modifyIORef appRef f

getAppM :: (DocumentClass d) => (App d -> a) -> AppM d a
getAppM f = ask >>= \appRef -> liftIO $ readIORef appRef >>= return . f

askApp :: (DocumentClass d) => AppM d (App d)
askApp = ask >>= liftIO . readIORef

-- NOTE: Safe.
-- Ищет элемент среди установленных структур.
askForElement :: forall frame d. (DocumentClass d, FrameClass frame)
              => (frame Element -> Element) -> AppM d (Maybe Element)
askForElement f = getAppM appFrames >>= \frameWrappers ->
  let trFrame       = typeRep (Proxy :: Proxy (frame Element))
  in return $ case (getMatchedFrame trFrame $ S.toList frameWrappers) of
                Nothing               -> Nothing
                -- Компилятор не позволяет применить функцию к полученной структуре, так
                -- как не имеет возможности проверить совпадение типов. Поэтому применяем ее
                -- динамически (runtime-ошибки быть не может).
                Just (AnyFrame frame) -> fromDynamic (toDyn f `dynApp` toDyn frame) :: Maybe Element
  where getMatchedFrame _ [] = Nothing
        getMatchedFrame trFrame (frameWrapper@(AnyFrame frame):xs)
         | eqTypeRep trFrame (typeOf frame) = Just frameWrapper
         | otherwise                        = getMatchedFrame trFrame xs
        eqTypeRep a b = typeRepFingerprint a == typeRepFingerprint b

withFrame :: (DocumentClass doc, FrameClass frame) => frame Text
                                                   -> (frame Element -> AppM doc a)
                                                   -> AppM doc a
withFrame builder behavior = do
  doc <- getAppM appDoc
  (releaser, frame) <- liftIO (initFrame doc builder)
  let frameWrapper = AnyFrame frame
  modifyAppM $ \app -> let oldFrames = appFrames app
                       in app { appFrames = S.insert frameWrapper oldFrames }
  behavior frame

type AppM doc a = ReaderT (IORef (App doc)) IO a

data App doc = App
  { appDoc        :: doc
  , appWin        :: Window
  , appSock       :: Socket
  , appAuthorized :: MVar Bool
  , appFrames     :: Set AnyFrame }

-- Stages Description ----------------------------------------------------------

data LoginStage =
    SignInStage
  | SignUpStage
  | RecoveryStageEmail
  | RecoveryStageKey
  | RecoveryStageChangePassw
  deriving (Data, Typeable, Show)

instance Default LoginStage where
  def = SignInStage

-- Data related ----------------------------------------------------------------

defaultPut :: (Binary a, Foldable t) => t a -> Put
defaultPut = Bin.putList . toList
