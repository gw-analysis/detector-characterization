module RecursiveContents
( getRecursiveContents
, getCurrentDirectory
, genFileList
) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    name <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".DS_Store"]) name
    paths <- forM properNames $ \tmpname -> do
      let path = topdir </> tmpname
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveContents path
        else return [path]
    return (concat paths)

genFileList :: FilePath -> FilePath -> IO()
genFileList fileName absDir = do
    contents <- getRecursiveContents absDir
    writeFile fileName $ unlines contents

