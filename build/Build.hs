{-# LANGUAGE OverloadedStrings #-}

module Build where

import qualified System.Directory as Dir  (getDirectoryContents
                                          ,createDirectoryIfMissing
                                          ,getCurrentDirectory)
import System.FilePath.Posix              (pathSeparator)
import System.Info                        (os, arch)
import Text.Regex.Posix                   ((=~))
import qualified Data.Text        as Text (isSuffixOf, pack)

type CabalContent = String
type CurrentDir   = String

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
createAppDir :: CurrentDir -> IO FilePath
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
