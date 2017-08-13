module GUI.General
  ( bind, as, build, wrap
  , hdrText, formText, pInp, sInp, btnLink, btnImportant, invalidBox, additional
  , short, getElemById ) where

--------------------------------------------------------------------------------
-- Содержит часто используемые элементы, а также
-- специальные функции, сокращающие шаблонный
-- код, получающийся при написании графических форм.
--------------------------------------------------------------------------------

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events (click)

import qualified Data.Maybe as M (fromJust)

--Attr setters------------------------------------------------------------------

setClass class' = set (attr "class") class'
setId    id'    = set (attr "id")    id'

--Builders----------------------------------------------------------------------

-- | Связывает кнопку с определенным действием.
bind :: UI Element -> UI () -> UI Element
bind btn' fun = do
  btn <- btn'
  on Events.click btn $ \_ -> fun
  return btn

-- | Присваивает объекту идентификатор.
as :: UI Element -> String -> UI Element
as el id' = el # setId id'

-- | Строит форму.
build :: String -> [UI Element] -> UI Element
build id' elems = do
  form <- Elems.div #  setClass "main-div"
                    #  setId    id'
                    #+ elems
  Elems.center # set children [ form ]

wrap :: [UI Element] -> UI Element
wrap elems = Elems.div #+ elems

short :: UI Element -> UI Element
short el = el # setClass "short"

--Patterns----------------------------------------------------------------------

hdrText :: String -> UI Element
hdrText text' = Elems.h1 # set text text'
                         # setClass "hdr-text"

formText :: String -> UI Element
formText text' = Elems.h2 # set text text'
                          # setClass "form-text"

pInp :: String -> UI Element
pInp name = Elems.input # set (attr "placeholder") name
                        # set (attr "type") "password"

sInp :: String -> UI Element
sInp name = Elems.input # set (attr "placeholder") name
                        # set (attr "type") "simple"

btnLink :: String -> UI Element
btnLink name = Elems.button # set text name
                            # setClass "btn-link"

btnImportant :: String -> UI Element
btnImportant name = Elems.button # set text name
                                 # setClass "btn-important"

invalidBox :: UI Element
invalidBox = Elems.div # setId "invalid-input-text"

additional :: [UI Element] -> UI Element
additional btns = Elems.div #+ btns
                            # setClass "additional-btns"

--Other-------------------------------------------------------------------------

getElemById :: Window -> String -> UI Element
getElemById w id' = do
  mElem <- getElementById w id'
  return (M.fromJust mElem)
