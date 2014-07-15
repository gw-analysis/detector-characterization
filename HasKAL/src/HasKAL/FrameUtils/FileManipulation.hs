module HasKAL.FrameUtils.FileManipulation
( getRecursiveFileSystem
, getCurrentDirectory
, genFileList
, extractstartGPStime
) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import System.FilePath ((</>))
import Data.List.Split (splitOn)

getRecursiveFileSystem:: FilePath -> IO [FilePath]
getRecursiveFileSystem topdir = do
    name <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".DS_Store"]) name
    paths <- forM properNames $ \tmpname -> do
      let path = topdir </> tmpname
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveFileSystem path
        else return [path]
    return (concat paths)

genFileList :: FilePath -> FilePath -> IO()
genFileList fileName absDir = do
    contents <- getRecursiveFileSystem absDir
    writeFile fileName $ unlines contents

extractstartGPStime :: String -> Integer
extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer
