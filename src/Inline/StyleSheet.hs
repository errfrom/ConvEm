{-# LANGUAGE OverloadedStrings #-}

module Inline.StyleSheet
 ( loadHtml, readHtml, appendHtml ) where

import Language.Haskell.TH                                      (ExpQ, Exp(LitE), Lit(StringL))
import Data.String.Utils                                        (replace, split, join)
import Text.Regex                                               (mkRegex, subRegex)
import Text.HTML.Parser                                         (Token(..), Attr(..))
import System.FilePath.Posix                                    ((</>))
import Graphics.UI.Gtk.WebKit.DOM.Document                      (DocumentClass)
import qualified Text.HTML.Parser                    as HtmlParser
import qualified Language.Haskell.TH                 as TH      (runIO)
import qualified Data.Text.Lazy                      as LText   (pack, unpack)
import qualified Data.Text                           as Text    (pack, unpack)
import qualified System.FilePath.Posix               as Posix   (replaceFileName)
import qualified System.Directory                    as Dir     (getCurrentDirectory)
import qualified Graphics.UI.Gtk.WebKit.DOM.Document as Doc
import qualified Graphics.UI.Gtk.WebKit.DOM.Element  as Element (setInnerHTML)
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLElement as HTMLElement (insertAdjacentHTML)

type Content = String

asStr :: (Monad m) => m String -> m Exp
asStr str = str >>= (return . LitE . StringL)

optimizeCss :: Content -> Content
optimizeCss = deleteMultipleSpaces . deleteComments . deleteNewLines
  where deleteNewLines         = replace "\n" ""
        deleteMultipleSpaces s = subRegex (mkRegex " +") s " "
        deleteComments       s =
          let endingWithComment   = split "*/" s
              commentStartRegex   = mkRegex "/[*].*$"
              deleteComment     s = subRegex commentStartRegex s ""
          in  join "" (map deleteComment endingWithComment)

-- Токены, определяющие тег 'link', указывающий на CSS.
-- Заменяет тегом 'style', содержащим весь контент, на
-- который ссылался 'link'.
replaceLinkByInline :: FilePath -> Token -> IO [Token]
replaceLinkByInline pathHtml token@(TagOpen _ attrs)
 |isCssLink attrs = case (getHrefAttrs attrs) of
    [Attr _ cssPath] ->
      let cssPathFull = Posix.replaceFileName pathHtml (Text.unpack cssPath)
      in do
        cssContent <- readFile cssPathFull
        return [ TagOpen "style" [(Attr "type" "text/css")]
               , ContentText (Text.pack $ optimizeCss cssContent)
               , TagClose "style" ]
    _ -> return [token]
 |otherwise = return [token]
  where getHrefAttrs = filter $ \(Attr name _) -> name == "href"
        isCssLink    = elem (Attr "type" "text/css")
replaceLinkByInline _ token = return [token]

-- Лениво обходит список токенов, заменяя все теги 'link' на их содержание.
-- Возвращает два списка: в первом - содержание тега head, во втором - остальное.
modifyTokensWithInlining :: FilePath -> [Token] -> IO ([Token], [Token])
modifyTokensWithInlining _ [] = return $ mempty ()
modifyTokensWithInlining pathHtml tokens@(token:xs) =
  let nextModified          = modifyTokensWithInlining pathHtml xs
      tokenWithNextModified = mappend (token:[], []) <$> nextModified
  in case token of
    TagOpen tagName _ -> if (tagName == "link")
                           then do replacedLinks <- replaceLinkByInline pathHtml token
                                   mappend (replacedLinks, []) <$> nextModified
                           else tokenWithNextModified
    TagClose tagName ->  if (tagName == "head")
                           then return (token:[], xs)
                           else tokenWithNextModified
    _                ->  tokenWithNextModified

getHtmlFullPath :: FilePath -> IO FilePath
getHtmlFullPath pathHtml = do
  dirCurrent <- Dir.getCurrentDirectory
  return (dirCurrent </> "static/html" </> pathHtml)

readHtml :: FilePath -> ExpQ
readHtml pathHtml = (asStr . TH.runIO) (getHtmlFullPath pathHtml >>= readFile)

appendHtml :: (DocumentClass doc) => doc -> FilePath -> String -> IO ()
appendHtml doc pathHtml htmlContents =
  let htmlTokens   = HtmlParser.parseTokensLazy (LText.pack htmlContents)
      headContents = (init . tail) htmlTokens -- Убираем head теги.
  in do
    htmlFullPath <- getHtmlFullPath pathHtml
    (headTokens, bodyTokens) <- modifyTokensWithInlining htmlFullPath htmlTokens
    let headContents = (toHtml . init . tail) headTokens
        bodyContents = toHtml bodyTokens
        htmlPosition = "beforeend"
    (Just elHead) <- Doc.getHead doc
    (Just elBody) <- Doc.getBody doc
    HTMLElement.insertAdjacentHTML elHead htmlPosition headContents
    HTMLElement.insertAdjacentHTML elBody htmlPosition bodyContents
    where toHtml = LText.unpack
                 . HtmlParser.renderTokens
                 . HtmlParser.canonicalizeTokens

-- Встраивает в HTML весь CSS контент, определенный тегами 'link',
-- предварительно его оптимизировав. Эти действия производятся на этапе
-- компиляции. Генерирует код, который подгружает полученный файл в GTK.
loadHtml :: FilePath -> ExpQ
loadHtml = asStr . TH.runIO . modifyHtml
  where modifyHtml pathHtml = do
          htmlFullPath <- getHtmlFullPath pathHtml
          htmlContent  <- readFile htmlFullPath
          let htmlTokens = HtmlParser.parseTokensLazy (LText.pack htmlContent)
          modifiedTokens <- modifyTokensWithInlining htmlFullPath htmlTokens
          return $ ( LText.unpack
                   . HtmlParser.renderTokens
                   . HtmlParser.canonicalizeTokens
                   . \tokens -> fst tokens ++ snd tokens ) modifiedTokens
