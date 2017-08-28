{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main.Login.Manager
  ( initForms ) where

import Graphics.UI.Threepenny.Core
import qualified Server.General as Server (initSocket)
import Main.Login.GUI
import Main.Login.Auth                    ()
import Types.General                      (SocketType(..))
import Types.Hierarchy hiding             (Callable(..), Checkable(..)
                                          ,Manageable(..), Form(..))

instance NonfunForm Start where
  nonfunForm Start sock = do
    build "start-form"
     [ wrap [ add (LblHeader "Приветствуем вас!")
            , add (LblDesc   "Извольте насладиться ощущением прогрессивного общения.") ]
     , wrap [ switch sock Auth (add $ BtnImportant "Вперед") ]]

-- | Инициализирует клиентский сокет и начальную форму.
-- Последущие формы чередуются функцией switch.
-- Также позволяет применять оформление одновременно ко
-- всем формам.
initForms :: UI Element
initForms = do
  sock <- liftIO (Server.initSocket ClientSocket)
  win  <- askWindow
  getBody win #+ [ add (Header "header.png")
                 , nonfunForm Start sock ]
