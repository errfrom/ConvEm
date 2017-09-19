{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}

module Templates.Inline
  ( Selector(..)
  , inlineCSS, inlineHTML, inlineSVG, inlineJS ) where

import Graphics.UI.Threepenny.Core   as UI hiding (split)
import Language.Haskell.TH                 (ExpQ, Exp(LitE), Lit(StringL))
import qualified Language.Haskell.TH as TH (runIO, appE)
import Data.String.Utils                   (replace, split, join)
import Text.Regex                          (mkRegex, subRegex)


data InlineOption = InlineCSS | InlineHTML
data Selector     = SelClass  | SelId | SelTag
type Value        = String
type Content      = String

asStr :: (Monad m) => m String -> m Exp
asStr str = str >>= (return . LitE . StringL)

optimizeCSS :: Content -> Content
optimizeCSS = deleteMultipleSpaces . deleteComments . deleteNewLines
  where deleteNewLines         = replace "\n" ""
        deleteMultipleSpaces s = subRegex (mkRegex " +") s " "
        deleteComments       s =
          let endingWithComment   = split "*/" s
              commentStartRegex   = mkRegex "/[*].*$"
              deleteComment     s = subRegex commentStartRegex s ""
          in  join "" (map deleteComment endingWithComment)

optimizeJS :: Content -> Content
optimizeJS = replace "\n" ""

inline :: InlineOption -> FilePath -> Selector -> Value -> ExpQ
inline inlineOption path selType selValue =
  let content   = TH.runIO (readFile path)
      optimized = optimizeContent inlineOption content
      funInline = buildInlineFun  inlineOption selType selValue
  in  TH.appE funInline (asStr optimized)
  where optimizeContent InlineCSS content = optimizeCSS <$> content
        optimizeContent _         content = content
        buildInlineFun inlineOption selType selValue =
          let cssSelector = genSel selType selValue
              jsMethod    = genJSMethod inlineOption
              jsCode      = "$('" ++ cssSelector ++ "')." ++ jsMethod ++ "(%1)"
              fun         = [e| \jsCode content ->
                            (UI.runFunction . flip UI.ffi content) jsCode |]
          in  TH.appE fun (asStr $ return jsCode)
        genSel SelClass val = "." ++ val
        genSel SelId    val = "#" ++ val
        genSel  _       val = val
        genJSMethod InlineCSS  = "text"
        genJSMethod _          = "html"

inlineCSS, inlineHTML, inlineSVG :: FilePath -> Selector -> Value -> ExpQ

inlineCSS  path selType selValue = inline InlineCSS  path selType selValue
inlineHTML path selType selValue = inline InlineHTML path selType selValue
inlineSVG                        = inlineHTML

inlineJS :: FilePath -> ExpQ
inlineJS path =
  let content   = TH.runIO (readFile path)
      optimized = fmap optimizeJS content
  in TH.appE [e| (UI.runFunction . UI.ffi) |] (asStr optimized)
