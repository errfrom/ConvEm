{-# LANGUAGE MultiParamTypeClasses #-}

module Main.Login.Decls () where

import Graphics.UI.Threepenny.Core              (UI)
import Types.Client
import Types.General                            (RecStage(..), Stage(..))
import Types.Results
import qualified Main.Login.Logic.Auth as LAuth (auth)
import qualified Main.Login.GUI.Auth   as GAuth (handleAuth)
import qualified Main.Login.GUI.Forms  as Forms (startForm, authForm, regForm
                                                ,recoveryForm_Email)


instance HasCallFunction Stage AuthResult where
  call Auth sock (inpEmail:inpPassw:_) =
    LAuth.auth sock inpEmail inpPassw

instance CanBeHandled Stage AuthResult where
  handle Auth sock = GAuth.handleAuth sock

instance Form Stage AuthResult where
  form Auth sock = Forms.authForm sock
