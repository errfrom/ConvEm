{-# OPTIONS_GHC -fno-warn-missing-signatures                 #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE PatternSynonyms      #-}

module Graphics.Data.Frames
  ( HeaderFrame(..), HeaderFrameData
  , CountryOption(..), PhoneMask(..), EntryFrameData(..)
  , EntryCodeFrame(..) ) where

import Control.Lens
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

data PhoneMask = PhoneMask [Int] | NoPhoneMask

data CountryOption = CountryOption
  { cCode  :: Text
  , cName  :: String
  , cpMask :: PhoneMask }

pattern BELARUS a = CountryOption "+375" a (PhoneMask [2, 3, 2, 2])
pattern RUSSIA  a = CountryOption "+7"   a (PhoneMask [3, 3, 2, 2])
pattern UKRAINE a = CountryOption "+380" a (PhoneMask [2, 3, 2, 2])
pattern USA     a = CountryOption "+1"   a (PhoneMask [3, 3, 4])
pattern FRANCE  a = CountryOption "+33"  a (PhoneMask [3, 3, 3])
pattern ISRAEL  a = CountryOption "+972" a (PhoneMask [1, 3, 4])

data EntryFrameData = EntryFrame
  { txtHeader  ::  Text
  , txtDesc    :: [Text]
  , defOption  ::  Text
  , txtErrors  :: [Text]
  , inpSearch  ::  Text
  , inpPhone   ::  Text
  , cOptions  :: [CountryOption] }
  deriving (Typeable)

instance Default EntryFrameData where
  def = EntryFrame "Welcome back" [ "Enter your phone number", "and press 'Enter' to continue." ]
                   "Choose the country"
                   [] "Search..." "Your phone number" [ BELARUS "Belarus"
                                                      , RUSSIA  "Russia"
                                                      , UKRAINE "Ukraine"
                                                      , USA     "United States"
                                                      , FRANCE  "France"
                                                      , ISRAEL  "Israel" ]

instance LocalizedClass EntryFrameData where
  translate ENG _ = def
  translate RUS _ = EntryFrame "Добро пожаловать" [ "Введите ваш номер телефона"
                                                  , "и нажмите 'Enter' для прожолжения."]
                               "Выберите страну"
                               [] "Поиск.." "Номер телефона" [ BELARUS "Беларусь"
                                                             , RUSSIA  "Россия"
                                                             , UKRAINE "Украина"
                                                             , USA     "США"
                                                             , FRANCE  "Франция"
                                                             , ISRAEL  "Израиль" ]

-- Entry code frame --------------------------------------------------------------------------------

data EntryCodeFrame = EntryCodeFrame
  { ecHeader :: Text
  , ecPhone  :: Text }
  deriving (Typeable)

instance Default EntryCodeFrame where
  def = EntryCodeFrame "Enter the code have been sent to" ""

instance LocalizedClass EntryCodeFrame where
  translate ENG EntryCodeFrame{ ecPhone = oldPhone } = def { ecPhone = oldPhone }
  translate RUS EntryCodeFrame{..} = EntryCodeFrame "Код отправлен на номер" ecPhone
