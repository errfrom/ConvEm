{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import qualified Init                    (initInterface)

import Templates.Build   (generateRestoring)
import System.Directory  (createDirectoryIfMissing)
import System.IO         (writeFile)
import Data.String.Utils (replace)


$(generateRestoring)

main :: IO ()
main = do
  restoreStatic -- см. Templates.Build.generateRestoring
  Init.initInterface
