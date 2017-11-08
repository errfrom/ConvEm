{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.General
  ( LoginStage(..)
  , defaultPut
  , AppM, App(..), modifyAppM, getAppM, askApp
  , module Frames ) where

--------------------------------------------------------------------------------
-- Объявления, часто использующиеся в других модулях.
--------------------------------------------------------------------------------

import Control.Concurrent                  (MVar)
import Control.Monad.Trans.Reader          (ReaderT, ask)
import Control.Monad.IO.Class              (liftIO)
import Data.Typeable                       (Typeable)
import Data.Foldable                       (toList)
import Data.Default                        (Default, def)
import Data.Data                           (Data)
import Data.Binary                         (Binary(..), Put)
import Data.IORef                          (IORef, modifyIORef, readIORef)
import Data.Set                            (Set)
import Network.Socket                      (Socket)
import Graphics.UI.Gtk                     (Window)
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Types.Frames          as Frames
import qualified Data.Binary as Bin        (putList)

modifyAppM :: (DocumentClass d) => (App d -> App d) -> AppM d ()
modifyAppM f = ask >>= \appRef -> liftIO $ modifyIORef appRef f

getAppM :: (DocumentClass d) => (App d -> a) -> AppM d a
getAppM f = ask >>= \appRef -> liftIO $ readIORef appRef >>= return . f

askApp :: (DocumentClass d) => AppM d (App d)
askApp = ask >>= liftIO . readIORef

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
