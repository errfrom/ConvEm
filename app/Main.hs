{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import qualified Init                    (initInterface)
import qualified System.Directory as Dir (getCurrentDirectory
                                         ,createDirectory)
import qualified System.IO        as IO  (FilePath, writeFile)
import           System.FilePath.Posix   (pathSeparator)

import Templates.Build   (generateRestoring)
import System.Directory  (createDirectoryIfMissing)
import System.IO         (writeFile)
import Data.String.Utils
--------------------------------------------------------------------------------

sep = pathSeparator : []
(+++) a b = a ++ "\n" ++ b

$(generateRestoring)

main :: IO ()
main = do
  restoreStatic
  Init.initInterface
