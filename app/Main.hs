{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main where

import qualified Init  (initInterface)
import Templates.Build (build)

$(build)

main :: IO ()
main = do
  restoreStatic -- см. Templates.Build.generateRestoring
  Init.initInterface
