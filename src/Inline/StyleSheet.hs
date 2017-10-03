{-# LANGUAGE OverloadedStrings #-}

module Inline.StyleSheet
 ( loadHtml ) where

import Language.Haskell.TH                      (ExpQ, Exp(LitE), Lit(StringL))
import Data.String.Utils                        (replace, split, join)
import Text.Regex                               (mkRegex, subRegex)
import Text.HTML.Parser                         (Token(..), Attr(..))
import qualified Text.HTML.Parser      as HtmlParser
import qualified Language.Haskell.TH   as TH    (runIO)
import qualified Data.Text.Lazy        as LText (pack, unpack)
import qualified Data.Text             as Text  (pack, unpack)
import qualified System.FilePath.Posix as Posix (replaceFileName)

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
    [Attr _ cssPath] -> do
      let cssPathFull = Posix.replaceFileName pathHtml (Text.unpack cssPath)
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
modifyTokensWithInlining :: FilePath -> [Token] -> IO [Token]
modifyTokensWithInlining _ [] = return []
modifyTokensWithInlining pathHtml tokens@(token:xs) =
  let modifyNext = modifyTokensWithInlining pathHtml xs
  in case token of
    TagOpen tagName _ ->
      if (tagName == "link")
        then mappend (replaceLinkByInline pathHtml token) modifyNext
        else fmap (token:) modifyNext
    TagClose tagName -> -- To be more lazy =)
      if (tagName == "head") -- Теги 'link' не могут находится в 'body'.
        then return tokens
        else fmap (token:) modifyNext
    _                -> fmap (token:) modifyNext

-- Встраивает в HTML весь CSS контент, определенный тегами 'link',
-- предварительно его оптимизировав. Эти действия производятся на этапе
-- компиляции. Генерирует код, который подгружает полученный файл в GTK.
loadHtml :: FilePath -> ExpQ
loadHtml = asStr . TH.runIO . modifyHtml
  where modifyHtml pathHtml = do
          htmlContent <- readFile pathHtml
          let htmlTokens = HtmlParser.parseTokensLazy (LText.pack htmlContent)
          modifiedTokens <- modifyTokensWithInlining pathHtml htmlTokens
          return $ ( LText.unpack
                   . HtmlParser.renderTokens
                   . HtmlParser.canonicalizeTokens) modifiedTokens
