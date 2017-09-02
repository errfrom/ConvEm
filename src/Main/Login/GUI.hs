{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main.Login.GUI
  ( switch, bind, as, build, row, wrap, short, additional ) where

--------------------------------------------------------------------------------
-- Содержит часто используемые элементы, а также
-- специальные функции, сокращающие шаблонный
-- код, получающийся при написании графических форм.
--------------------------------------------------------------------------------

import Network.Socket                                      (Socket)
import Graphics.UI.Threepenny.Core               hiding    (row)
import qualified Graphics.UI.Threepenny.Elements as Elems
import qualified Graphics.UI.Threepenny.Events   as Events (click, valueChange)
import qualified Utils                                     (removeClass
                                                           ,getElemType)
import Types.Hierarchy
import Types.Elems


--Attr setters------------------------------------------------------------------

setId, setType, setText :: String -> UI Element -> UI Element
setId   id'    = set (attr "id")    id'
setType type'  = set (attr "type")  type'
setText text'  = set text text'

--Defaults----------------------------------------------------------------------

instance DefaultDecl InputKind  where
  def _ = handleFilled =<< Elems.input # set (attr "maxlenght") "80"
                                       # setType "simple"

--Concretized Elements----------------------------------------------------------

instance BuildableElem ButtonKind where
  add (BtnImportant text') = Elems.button # setText text'
                                          #. "btn-important"
  add (BtnLink text')      = Elems.button # setText text'
                                          #. "btn-link"

instance BuildableElem InputKind where
  add i@(InpSimple   text') = def i # set (attr "placeholder") text'
  add i@(InpEmail    text') = def i # set (attr "placeholder") text'
  add i@(InpPassword text') = def i # setType "password"
                                    # set (attr "placeholder") text'

instance BuildableElem LabelKind where
  add (LblHeader text')  = Elems.h1  # setText text'
                                     #.  "hdr-text"
  add (LblDesc text')    = Elems.h2  # setText text'
                                     #. "form-text"
  add LblInvalid         = Elems.div # setId "invalid-input-text"

instance BuildableElem ImageKind where
  add (Image path)     = Elems.img # set (attr "src") ("/static/images/" ++ path)
  add (ImgHeader path) = do
    img  <- Elems.img # set (attr "src") ("/static/images/" ++ path)
                      # set (attr "draggable") "false"
    wrapper <- Elems.div #. "hdr-wrapper"
                         # set children [ img ]
    Elems.div #  set children [ wrapper ]
              #. "header"

--Builders----------------------------------------------------------------------

switch :: (Form s r) => Socket -> s -> UI Element -> UI Element
switch sock stage el = el >>= \el' -> worker el' >> return el'
  where worker el' = on Events.click el' $ \_ -> do
          window <- liftIO (getWindow el')
          clearWindow window
          getBody window #+ [ form stage sock ]

        clearWindow window = do
          [mainDiv] <- getElementsByClassName window "main-div"
          [center]  <- getElementsByTagName   window "center"
          mapM_ delete [ mainDiv, center ]

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
  mainDiv <- Elems.div #. "main-div"
                       #  setId    id'
                       #+ elems
  Elems.center # set children [ mainDiv ]

wrap :: [UI Element] -> UI Element
wrap elems = Elems.div #+ elems

short :: UI Element -> UI Element
short el = Elems.div #+ [ el #. "short" ]

row :: UI Element -> UI Element -> UI Element
row elem1 elem2 = Elems.div #. "row"
                            #+ [ elem1, elem2 ]

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
    on Events.valueChange inp $ \val -> do
      phText <- callFunction (ffi "$(%1).attr('placeholder')" inp :: JSFunction String)
      phSize <- getTextWidth phText font
      worker val phSize font
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
