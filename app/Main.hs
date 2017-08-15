module Main where

--------------------------------------------------------------------------------
import qualified Init                    (initInterface)
import qualified System.Directory as Dir (getCurrentDirectory
                                         ,createDirectory)
import qualified System.IO        as IO  (FilePath, writeFile)
import           System.FilePath.Posix   (pathSeparator)
--------------------------------------------------------------------------------

sep = pathSeparator : []
(+++) a b = a ++ "\n" ++ b

-- | Создает необходимые для работы программы файлы.
-- NOTE: С помощью скрипта сборки строки new и efregre заменяются на
--       действительные данные.
prepareAdditionalFiles :: IO ()
prepareAdditionalFiles = do
  current <- Dir.getCurrentDirectory
  let static = current ++ sep ++ ".static" ++ sep
      css    = static ++ "css" ++ sep
      fonts  = css ++ "fonts.css"
      login  = css ++ "login.css"
  Dir.createDirectory static
  Dir.createDirectory css
  IO.writeFile fonts "__FONTS__"
  IO.writeFile login "__LOGIN__"

main :: IO ()
main = do
  prepareAdditionalFiles
  Init.initInterface
