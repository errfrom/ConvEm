module Main.Login.GUI.General
  ( Concretized(..), ButtonKind(..), InputKind(..), LabelKind(..), ImageKind(..)
  , bind, as, build, row, wrap, short, additional ) where

--------------------------------------------------------------------------------
-- Содержит часто используемые элементы, а также
-- специальные функции, сокращающие шаблонный
-- код, получающийся при написании графических форм.
--------------------------------------------------------------------------------

import Graphics.UI.Threepenny.Core               hiding    (row)
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events (click, valueChange)
import System.IO                                           (FilePath)
import qualified Utils                                     (removeClass
                                                           ,getElemType)


--Attr setters------------------------------------------------------------------

setId   id'    = set (attr "id")    id'
setText text'  = set text text'

--Concretized Elements----------------------------------------------------------

data ButtonKind =
  BtnImportant String
 |BtnLink      String

data InputKind =
  InpSimple   String
 |InpPassword String

data LabelKind =
  LblHeader  String
 |LblDesc    String
 |LblInvalid

data ImageKind =
  Header FilePath
 |Image  FilePath

class Concretized t where
  add :: t -> UI Element

instance Concretized ButtonKind where
  add (BtnImportant text) = Elems.button # setText text
                                         #. "btn-important"
  add (BtnLink text)      = Elems.button # setText text
                                         #. "btn-link"

instance Concretized InputKind where
  add (InpSimple text)   = handleFilled =<< Elems.input # set (attr "placeholder") text
                                                        # set (attr "type") "simple"
                                                        # set (attr "maxlength") "80"
  add (InpPassword text) = handleFilled =<< Elems.input # set (attr "placeholder") text
                                                        # set (attr "type") "password"
                                                        # set (attr "maxlength") "80"

instance Concretized LabelKind where
  add (LblHeader text)  = Elems.h1  # setText text
                                    #.  "hdr-text"
  add (LblDesc text)    = Elems.h2  # setText text
                                    #. "form-text"
  add LblInvalid        = Elems.div # setId "invalid-input-text"

instance Concretized ImageKind where
  add (Image path)  = Elems.img # set (attr "src") ("/static/images/" ++ path)
  add (Header path) = do
    img  <- Elems.img # set (attr "src") ("/static/images/" ++ path)
    wrap <- Elems.div #. "hdr-wrapper"
                      # set children [ img ]
    Elems.div #  set children [ wrap ]
              #. "header"

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
  form <- Elems.div #. "main-div"
                    #  setId    id'
                    #+ elems
  Elems.center # set children [ form ]

wrap :: [UI Element] -> UI Element
wrap elems = Elems.div #+ elems

short :: UI Element -> UI Element
short el = el #. "short"

row :: [UI Element] -> UI Element
row elems = Elems.div #. "row"
                      #+ elems

additional :: [UI Element] -> UI Element
additional btns = Elems.div #+ btns
                            #. "additional-btns"

--Other-------------------------------------------------------------------------

-- | Рассчитывает, когда следует спрятать placeholder элемента input и
-- устанавливает соответствующее событие.
handleFilled :: Element -> UI Element
handleFilled inp =
  let font = "14px Roboto"
  in do
    phText <- callFunction (ffi "$(%1).attr('placeholder')" inp :: JSFunction String)
    phSize <- getTextWidth phText font
    on Events.valueChange inp $ \val -> worker val phSize font
    return inp
  where worker val phSize font =
          let maxSize      = 260 - phSize
              classFilled  = "filled"
          in do
            elType    <- Utils.getElemType inp
            textWidth <- case elType of
                           "password" -> do
                             startWidth <- getTextWidth "•" font
                             return $ (length val) * startWidth
                           _          -> (getTextWidth val font :: UI Int)
            if textWidth > (maxSize - 20) -- Небольшой отступ (px)
              then (return inp) #. classFilled
              else Utils.removeClass inp classFilled >> return inp

        getTextWidth txt font =
          let js = "function get_text_width() {"
                ++    "this.element = document.createElement('canvas');"
                ++    "this.context = this.element.getContext('2d');"
                ++    "this.context.font = '" ++ font ++ "';"
                ++    "return this.context.measureText('" ++ txt ++ "').width; }"
                ++ "get_text_width();"
          in (callFunction . ffi) js >>= return
