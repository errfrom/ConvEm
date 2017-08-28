{-# LANGUAGE OverloadedStrings #-}

module Templates.Build
  ( generateRestoring, createAppDir ) where


import qualified Language.Haskell.TH as TH
import qualified System.Directory    as Dir    (getDirectoryContents
                                               ,createDirectoryIfMissing
                                               ,doesDirectoryExist)
import System.FilePath.Posix                   (pathSeparator)
import System.Info                             (os, arch)
import System.FilePath.Posix                   (dropFileName)
import Text.Regex.Posix                        ((=~))
import qualified Data.Text           as Text   (isSuffixOf, pack)
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
generateRestoring = do
  fun <- TH.funD (TH.mkName "restoreStatic") [generateClause]
  return [fun]
  where generateClause = do
          -- Использование './' позволяет нам в дальнейшем
          -- создавать корректный путь к генерируемым файлам.
          dataFiles <- TH.runIO (walkDirectory "./static/css")
          genData   <- mapM getFileGenData dataFiles
          TH.clause [] (TH.normalB $ return (createExp genData)) []

        getFileGenData file =
          let newFilePath = SUtils.replace "./static" "./.static" file
          in do
            content <- TH.runIO (SUtils.replace "\n" "\\n" <$> readFile file)
            return (newFilePath, content)

        createExp ((file, content):xs) =
          -- TODO: Looks very bad -> rewrite via Quasi Quotes.
          let folder     = dropFileName file
              createDir' = TH.mkName "System.Directory.createDirectoryIfMissing"
              bind'      = TH.mkName ">>"
              writeF'    = TH.mkName "System.IO.writeFile"
              boolTrue   = TH.mkName "True"
              replace'   = TH.mkName "Data.String.Utils.replace"
          -- Трансформируется в:
          --   createDirectoryIfMissing {folder} >>
          --     writeFile {file} (replace "\\n" "\n" {content})
          in TH.InfixE
               (Just $ TH.AppE
                 (TH.AppE (TH.VarE createDir') (TH.ConE boolTrue))
                 (TH.LitE (TH.StringL folder)))
               (TH.VarE bind')
               (Just $ (TH.AppE
                 (TH.AppE (TH.VarE writeF') (TH.LitE (TH.StringL file)))
                 (TH.AppE
                   (TH.AppE
                     (TH.AppE (TH.VarE replace') (TH.LitE (TH.StringL "\\n")))
                     (TH.LitE (TH.StringL "\n")))
                   (TH.LitE (TH.StringL content)))))
