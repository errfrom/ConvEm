{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types.Elems
  ( ElemKind
  , ButtonKind(..), InputKind(..), LabelKind(..), ImageKind(..)
  , DefaultDecl(..), BuildableElem(..) ) where

--------------------------------------------------------------------------------
-- Типы данных и классы, позволяющие определять поведение элементов GUI.
--------------------------------------------------------------------------------

import Graphics.UI.Threepenny.Core (UI, Element)


data ButtonKind =
  BtnImportant String
 |BtnLink      String

data InputKind =
  InpSimple   String
 |InpEmail    String
 |InpPassword String

data LabelKind =
  LblHeader  String
 |LblDesc    String
 |LblInvalid

data ImageKind =
  ImgHeader FilePath
 |Image     FilePath

class ElemKind e where
instance ElemKind ButtonKind where
instance ElemKind InputKind  where
instance ElemKind LabelKind  where
instance ElemKind ImageKind  where

class (ElemKind k) => DefaultDecl k where
  def :: k -> UI Element

class (ElemKind k) => BuildableElem k where
  add :: k -> UI Element
