{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types.DBEntities where

import Control.Monad.Trans.Control  (MonadBaseControl)
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Logger         (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString              (ByteString)
import Data.Word                    (Word32)
import Database.Persist.TH
import Database.Persist.MySQL
import qualified Data.Text as LT (Text)

mkPersist sqlSettings { mpsPrefixFields   = False
                      , mpsEntityJSON     = Nothing } [persistLowerCase|
User sql=Users
  uId      Word32
  uEmail   LT.Text
  uName    LT.Text
  uSurname LT.Text
  uPassw   ByteString
  Primary uId uEmail
  deriving (Show)
|]

withDBConn :: (MonadBaseControl IO m, MonadIO m) => (SqlBackend -> ResourceT (NoLoggingT m) a) -> m a
withDBConn action = runNoLoggingT
                  . runResourceT
                  . withMySQLConn connectInfo $ action
  where connectInfo = defaultConnectInfo { connectPassword = "password"
                                         , connectDatabase = "convemdb" }
