module Logic.Recovery where

import Logic.General

data RecoveryResult =
  InvalidValues MistakeIn
 |SuccessfullySent
