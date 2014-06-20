module RecursiveContents
( getRecursiveContents
, getCurrentDirectory
--,
) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    name <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) name
    paths <- forM properNames $ \tmpname -> do
      let path = topdir </> tmpname
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveContents path
        else return [path]
    return (concat paths)



