{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Templates.Build
  ( build ) where

import qualified Language.Haskell.TH as TH
import qualified Data.ByteString     as BS     (readFile)
import qualified System.Directory    as Dir    (getDirectoryContents
                                               ,createDirectoryIfMissing
                                               ,doesDirectoryExist)
import System.FilePath.Posix                   (pathSeparator)
import System.Info                             (os, arch)
import System.FilePath.Posix                   (dropFileName, takeFileName)
import Text.Regex.Posix                        ((=~))
import qualified Data.Text           as Text   (isSuffixOf, isPrefixOf, pack)
import qualified Data.String.Utils   as SUtils (replace)


type CabalContent = String

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
          dataFiles <- TH.runIO (walkDirectory "./static/css") -- NOTE
          genData   <- mapM getFileGenData dataFiles
          createExp genData ["logo.png"]

        getFileGenData file =
          let newFilePath = SUtils.replace "./static" "./.static" file
          in do
            txtContent <- TH.runIO (readFile file)
            -- NOTE: Исключение зачем-то возвращается в виде текста.
            content    <- TH.runIO $
              if (Text.isPrefixOf "*** Exception:" $ Text.pack txtContent)
                then BS.readFile file >>= return . show
                else return (SUtils.replace "\n" "\\n" txtContent)
            return (newFilePath, content)

        createExp genData'@((file, _):nextData) ignoreList
         |elem (takeFileName file) ignoreList = createExp nextData ignoreList
         |otherwise =
            -- Трансформируется в:
            --   createDirectoryIfMissing {folder} >>
            --     writeFile {file} (replace "\\n" "\n" {content})
           let exps = map (\(f, content) -> worker f content (dropFileName f))
                          genData'
           in TH.appE [e| sequence |] (TH.listE exps)
           where -- TODO: Сделать поддержку бинарных файлов.
                 --       Легко реализуется расширением genData еще одним полем
                 --       с типом файла (бинарный, текстовый) и последующей
                 --       обработкой worker'ом.
                 worker file' content' folder' =
                   TH.infixE (Just $ createDirectory' folder')
                             [e| (>>) |]
                             (Just $ writeFile' file' (replace' content'))
                 asStr = (TH.litE . TH.StringL)
                 writeFile' f content'    =
                   TH.appE (TH.appE [e| writeFile |] $ asStr f) content'
                 replace' content'        =
                   TH.appE [e| SUtils.replace "\\n" "\n" |] (asStr content')
                 createDirectory' folder' =
                   TH.appE [e| Dir.createDirectoryIfMissing True |] (asStr folder')

-- | Основная функция, на этапе компиляции создающая
build :: TH.DecsQ
build = TH.runIO (createAppDir ".") >> generateRestoring
