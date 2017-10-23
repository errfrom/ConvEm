{-# LANGUAGE StrictData        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Form
  ( SwitchBtnToken(..), DescToken(..), FieldToken(..)
  , UIFormBuilder(..), buildForm
  , notifyError, hideError
  , basicFormSetup
  , UIHeight ) where

import Control.Monad.IO.Class              (liftIO)
import Control.Monad.Trans.Reader          (ReaderT(..), ask)
import Data.Word
import Data.Text                           (Text)
import Text.HTML.Parser                    (Token(..), Attr(..))
import Graphics.UI.Gtk.WebKit.DOM.Document (DocumentClass)
import Graphics.UI.Gtk.WebKit.DOM.Element
import Graphics.General                    (Id, onFocus, operateElemById, getInputs)
import Graphics.Data.Selectors
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLElement      as Element
import qualified Text.HTML.Parser                            as HtmlParser
import qualified Data.Text.Lazy                              as LT (toStrict)

class HTMLTokenized t where
  tokenize :: t -> [Token]

tokenizeBtnLink :: Id -> Text -> [Token]
tokenizeBtnLink btnId btnText =
   TagOpen "button" [ Attr "class" (unSel selBtnLink), Attr "id" btnId ]
 : ContentText btnText
 : TagClose "button" : []

-- 'SwitchBtn' Token -----------------------------------------------------------

data SwitchBtnToken = SwitchBtnToken Id Text

instance HTMLTokenized SwitchBtnToken where
  tokenize (SwitchBtnToken btnId btnText) = tokenizeBtnLink btnId btnText

-- 'Description' Token ---------------------------------------------------------

data DescToken =
  -- Определяют строку, содержащую только один элемент.
  OneLineDescText Text
 |OneLineDescBtn  Id Text
  -- Указывают, где должна начинаться и заканчиваться строка.
 |DescStart
 |DescEnd
  -- Представление одного элемента.
 |DescText Text
 |DescBtn  Id Text

instance HTMLTokenized DescToken where
  tokenize (DescBtn  id' btnText) = tokenizeBtnLink id' btnText
  tokenize (DescText text) = ContentText text : []

  tokenize DescStart = TagOpen  "div" [Attr "class" (unSel selTxtAdvice)] : []
  tokenize DescEnd   = TagClose "div" : []

  tokenize oneLine = wrapOneLine $ case oneLine of
    OneLineDescText text        -> DescText text
    OneLineDescBtn  id' btnText -> DescBtn id' btnText
    _                           -> undefined
    where wrapOneLine token = tokenizeMany [DescStart, token, DescEnd]

-- 'Field' Token ---------------------------------------------------------------

data FieldToken =
   SingleField Id Text
  |FieldPair   Id Text Id Text

instance HTMLTokenized FieldToken where
  tokenize (SingleField id' text) =
    let typeAttr = if id' == (unSel selInpPassw)
                   || id' == (unSel selInpPasswRepeat)
                   then Attr "type" "password" : []
                   else []
    in TagOpen "input" ([ Attr "id" id', Attr "placeholder" text ] ++ typeAttr)
    :  TagClose "input" : []
  tokenize (FieldPair fId' fText sId' sText) =
      TagOpen "div" [ Attr "class" (unSel selBoxFieldPair) ]
   :  wrapWithDiv (unSel selBoxFieldFirst)  (tokenize $ SingleField fId' fText)
   ++ wrapWithDiv (unSel selBoxFieldSecond) (tokenize $ SingleField sId' sText)
   ++ (TagClose "div" : [])
   where wrapWithDiv cls a = TagOpen "div" [ Attr "class" cls ]
                           : a ++ (TagClose "div" : [])

-- Form Building ---------------------------------------------------------------

type FormHeader = Text

-- Внешний вид любой формы однозначно определяется значениями
-- каждого поля UIFormBuilder.
data UIFormBuilder = UIFormBuilder
  { formHeader ::   FormHeader
  , switchBtns :: [ SwitchBtnToken ]
  , adviceText :: [ DescToken      ]
  , formFields :: [ FieldToken     ] }

renderTokensStrict :: [Token] -> Text
renderTokensStrict = LT.toStrict . HtmlParser.renderTokens

tokenizeMany :: (HTMLTokenized t) => [t] -> [Token]
tokenizeMany = concat . map tokenize

setInnerText :: (ElementClass elem) => Text -> elem -> IO ()
setInnerText text el = Element.setInnerText (Element.castToHTMLElement el)
                                            (Just text)
class UIBuildable t where
  build :: (DocumentClass doc) => t -> ReaderT doc IO ()

instance UIBuildable [SwitchBtnToken] where
  build switchBtns =
    let html = (renderTokensStrict . tokenizeMany) switchBtns
    in  operateElemById selBoxNavigation (flip setInnerHTML $ Just html)

instance UIBuildable FormHeader where
  build formHeader = operateElemById selTxtHeader (setInnerText formHeader)

instance UIBuildable [DescToken] where
  build descText =
    let descHtml = (renderTokensStrict . tokenizeMany) descText
    in  operateElemById selBoxAdvice (flip setInnerHTML $ Just descHtml)

instance UIBuildable [FieldToken] where
  build formFields =
    let fieldsHtml = (renderTokensStrict . tokenizeMany) formFields
    in  operateElemById selBoxInputs (flip setInnerHTML $ Just fieldsHtml)

instance UIBuildable UIFormBuilder where
  build UIFormBuilder{..} = build formHeader >> build adviceText
                                             >> build formFields
                                             >> build switchBtns

-- NOTE: Should I create UIFormData typeclass?
buildForm :: (DocumentClass doc) => UIFormBuilder -> ReaderT doc IO UIHeight
buildForm uiForm = build uiForm >> adaptFormSize uiForm

-- Form Sizing -----------------------------------------------------------------

type UIHeight = Word16

-- Рассчитывает высоту формы, складывая значения высот ее элементов.
getFormHeight :: UIFormBuilder -> UIHeight
getFormHeight uiForm =
  let emptyFormHeight = 50
  in  emptyFormHeight + getFieldsHeight uiForm + (getDescHeight . adviceText) uiForm
  where getFieldsHeight = fromIntegral . (* 50) . length . formFields

getDescHeight :: [DescToken] -> UIHeight
getDescHeight = (* 21) . countDescRows
  where countDescRows ((OneLineDescText _):xs)  = 1 + countDescRows xs
        countDescRows ((OneLineDescBtn _ _):xs) = 1 + countDescRows xs
        countDescRows (DescEnd:xs)              = 1 + countDescRows xs
        countDescRows (_:xs)                    = countDescRows xs
        countDescRows []                        = 0

setFormHeight :: (DocumentClass doc) => UIHeight -> ReaderT doc IO ()
setFormHeight formHeight = operateElemById selForm (worker formHeight)
  where worker formHeight' form =
          let formHeight = show formHeight'
              -- TODO: Use some CSS parser.
              css        = "height: " ++ formHeight ++
                           "; top:calc(50% - " ++ formHeight ++ "px/2);"
          in setAttribute form ("style" :: String) css -- NOTE: removeAttribute?

adaptFormSize :: (DocumentClass doc) => UIFormBuilder -> ReaderT doc IO UIHeight
adaptFormSize uiForm =
  let formHeight = getFormHeight uiForm
  in  setFormHeight formHeight >> return formHeight

-- General Functions -----------------------------------------------------------

-- Показывает ошибку пользователю, а также корректирует высоту формы в
-- зависмости от размера текста ошибки.
notifyError :: (DocumentClass doc) => UIHeight -> [DescToken] -> ReaderT doc IO ()
notifyError formHeight errorText = do
  operateElemById selBoxError (worker errorText)
  setFormHeight (formHeight + getDescHeight errorText + 25)
  where worker errorText boxError =
          let errorHtml = (renderTokensStrict . tokenizeMany) errorText
          in do
            setInnerHTML boxError (Just errorHtml)
            setClassName boxError (unSel selShowError)

-- Прячет ошибку.
-- NOTE: Правило (hideError . notifyError) == id не выполняется в силу того,
--       что hideError всего лишь удаляет класс selShowError. При этом
--       все еще остается ошибка. Позже она может быть заменена другой.
--       Это сделано в целях производительности.
hideError :: (DocumentClass doc) => UIHeight -> ReaderT doc IO ()
hideError formHeight = do
  operateElemById selBoxError $ \boxError -> setClassName boxError ("" :: String)
  setFormHeight formHeight

basicFormSetup :: (DocumentClass doc) => UIHeight -> ReaderT doc IO ()
basicFormSetup uiHeight = ask >>= \doc -> liftIO $ do
  inputs <- getInputs doc
  mapM_ (flip onFocus $ runReaderT (hideError uiHeight) doc) inputs
