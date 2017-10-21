{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Graphics.Data.Dialogs
  ( ConnErrDialogData(..), connErrDialogData ) where

import Data.Text (Text)

data ConnErrDialogData = ConnErrDialogData
  { dataBtnRetryActive    :: Text
  , dataBtnRetryExecuting :: Text
  , dataConnErrMessage    :: Text }

connErrDialogData :: ConnErrDialogData
connErrDialogData = ConnErrDialogData "Попробовать снова"
                                      "Выполнение..."
                                      "Возникла ошибка соединения."
