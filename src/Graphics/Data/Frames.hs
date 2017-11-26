{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Graphics.Data.Frames
  ( HeaderFrame(..), HeaderFrameData
  , CountryOption(..), EntryFrameData(..) ) where

import Data.Default  (Default(..))
import Data.Typeable (Typeable)
import Data.Text     (Text)
import Types.Frames  (Language(..), LocalizedClass(..))

-- Header frame ------------------------------------------------------------------------------------

data HeaderFrame a = HeaderFrame
  { btnQuit     :: a
  , btnSettings :: a
  , btnAbout    :: a }
  deriving (Typeable, Functor, Foldable, Traversable)

type HeaderFrameData = HeaderFrame Text

instance Default HeaderFrameData where
  def = HeaderFrame "Quit" "Settings" "About"

instance LocalizedClass HeaderFrameData where
  translate ENG _ = def
  translate RUS _ = HeaderFrame "Выход" "Настройки" "Conv'Em"

-- Entry frame -------------------------------------------------------------------------------------

data CountryOption = CountryOption Text String

data EntryFrameData = EntryFrame
  { txtHeader ::  Text
  , txtDesc   :: [Text]
  , txtErrors :: [Text]
  , inpSearch ::  Text
  , inpPhone  ::  Text
  , cOptions  :: [CountryOption] }
  deriving (Typeable)

instance Default EntryFrameData where
  def = EntryFrame "Welcome back" [ "Enter your phone number", "and press 'Enter' to continue." ]
                   [] "Search..." "Your phone number" []

instance LocalizedClass EntryFrameData where
  translate ENG _ = def
  translate RUS _ = EntryFrame "Добро пожаловать" [ "Введите ваш номер телефона"
                                                  , "и нажмите 'Enter' для прожолжения."]
                               [] "Поиск.." "Номер телефона" []
