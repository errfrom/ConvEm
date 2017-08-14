module Logic.Recovery where

import Logic.General
import Network.Mail.SMTP

data RecoveryResult =
  InvalidValues MistakeIn
 |SuccessfullySent
