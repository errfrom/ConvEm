{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Templates.Build
  ( build ) where

import qualified Language.Haskell.TH   as TH
import qualified Data.ByteString.Char8 as BS     (ByteString, pack, unpack
                                                 ,readFile, writeFile)
import qualified System.Directory      as Dir    (getDirectoryContents
                                                 ,createDirectoryIfMissing
                                                 ,doesDirectoryExist)
import System.FilePath.Posix                     (pathSeparator)
import System.Info                               (os, arch)
import System.FilePath.Posix                     (dropFileName, takeFileName)
import Text.Regex.Posix                          ((=~))
import qualified Data.Text             as Text   (isSuffixOf, isPrefixOf, pack)
import Data.Text.Encoding                        (decodeUtf8')
import qualified Data.String.Utils     as SUtils (replace)


type CabalContent = String
data FileType = TextType | BinaryType

pathSep :: String
pathSep = pathSeparator : []

-- | Основываясь на данных, представленных в Cabal файле,
-- возвращает текущую версию приложения.
getAppVersion :: CabalContent -> String
getAppVersion content =
  let versionPattern    = "version: *([0-9.]*)"     :: String
      ((_:version:_):_) = content =~ versionPattern :: [[String]]
  in version

-- | Название ОС и архитектура. Пример: linux-x86_64.
sysConf :: String
sysConf = os ++ "-" ++ arch

-- | Создает директорию с исполняемым файлом проекта.
createAppDir :: FilePath -> IO FilePath
createAppDir current =
  let appName    = "Sheer"
      nameSep    = "-"
  in do
    listFiles    <- Dir.getDirectoryContents current
    cabalContent <- readFile (getCabal listFiles)
    let appVersion = getAppVersion cabalContent
        appDirName = appName ++ nameSep ++ appVersion ++ nameSep ++ sysConf
        appDir     = current ++ pathSep ++ appDirName
    Dir.createDirectoryIfMissing False appDir
    return appDir
  where getCabal listFiles =
          case (filter isCabalFile listFiles) of
            [cabal] -> cabal
            []      -> error "Cabal файл не найден."
        isCabalFile = (Text.isSuffixOf ".cabal" . Text.pack)

-- | Рекурсивно обходит директорию, возвращая все найденные файлы.
walkDirectory :: FilePath -> IO [FilePath]
walkDirectory startDir = do
  startDirContents <- Dir.getDirectoryContents startDir
  worker startDir startDirContents
  where worker :: FilePath -> [FilePath] -> IO [FilePath]
        worker _ []             = return []
        worker dir (dirFile:xs)
         |elem dirFile [".", ".."] = worker dir xs
         |otherwise =
           let full = dir ++ pathSep ++ dirFile
           in do
             isDir <- Dir.doesDirectoryExist full
             if isDir
               then mappend <$> (walkDirectory full) <*> (worker dir xs)
               else (full:) <$> (worker dir xs)

generateRestoring :: TH.DecsQ
generateRestoring = [d| restoreStatic :: IO [()]
                        restoreStatic = $(generateBody) |]
  where generateBody = do
          -- Использование './' позволяет нам в дальнейшем
          -- создавать корректный путь к генерируемым файлам.
          dataFiles <- TH.runIO (walkDirectory "./static") -- NOTE
          genData   <- mapM getFileGenData dataFiles
          let dataFiltered = filter (\(file, _, _) ->
                not $ elem file ["./.static/images/logo.png"]) genData
          createExp dataFiltered

        getFileGenData file =
          let newFilePath = SUtils.replace "./static" "./.static" file
          in do
            TH.runIO $ do
              content <- BS.readFile file
              let contentType = case (decodeUtf8' content) of
                                  Left _  -> BinaryType
                                  Right _ -> TextType
              return (newFilePath, content, contentType)

        createExp genData' =
          let exps = flip map genData' (\(f, content, fType) ->
                     worker fType f content (dropFileName f))
          in TH.appE [e| sequence |] (TH.listE exps)
          where
            worker TextType file' content' folder' =
              worker_ TextType file' ((replace' . BS.unpack) content') folder'
            worker BinaryType file' content' folder' =
              worker_ BinaryType file' ((asStr . BS.unpack) content') folder'
            worker_ type' file' content' folder' =
              let content = TH.appE [| BS.pack |] content'
              in  TH.infixE (Just $ createDirectory' folder')
                            [e| (>>) |]
                            (Just $ writeFile' type' file' content)
            asStr    = (TH.litE . TH.StringL)
            writeFile' fType' f content' =
              let writeFun = case fType' of
                               TextType   -> [e| \x y -> writeFile x $ BS.unpack y |]
                               BinaryType -> [e| BS.writeFile |]
              in TH.appE (TH.appE writeFun $ asStr f) content'
            replace' content'        =
              TH.appE [e| SUtils.replace "\\n" "\n" |] (asStr content')
            createDirectory' folder' =
              TH.appE [e| Dir.createDirectoryIfMissing True |] (asStr folder')

-- | Основная функция, на этапе компиляции создающая
build :: TH.DecsQ
build = TH.runIO (createAppDir ".") >> generateRestoring
