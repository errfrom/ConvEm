module GUI.Elements.Input
  ( InputType(..)
  , simpleInput ) where

--------------------------------------------------------------------------------
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements       as Elems
--------------------------------------------------------------------------------

data InputType = Simple | Password

{-
instance Functor Element where
  fmap fun element
-} -- TODO

simpleInput :: String -> InputType -> UI Element
simpleInput placeHolder inputType =
  let inpType = case inputType of
                  Simple   -> "simple"
                  Password -> "password"
  in Elems.input # set (attr "placeholder") placeHolder
                 # set (attr "type") inpType
