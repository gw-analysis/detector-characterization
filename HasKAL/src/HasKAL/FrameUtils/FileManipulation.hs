module HasKAL.FrameUtils.FileManipulation
( getRecursiveFileSystem
, getCurrentDirectory
, genFileList
, extractstartGPStime
, extractDataLength
, extractstartGPStimefromFilename
, extractDataLengthfromFilename
) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import System.FilePath ((</>))
import Data.List (sort)
import Data.List.Split (splitOn)
import Numeric.LinearAlgebra
import Numeric.GSL.LinearAlgebra (fromFile, fileDimensions)
import System.IO.Unsafe (unsafePerformIO)

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
    return $ sort (concat paths)

genFileList :: FilePath -> FilePath -> IO()
genFileList fileName absDir = do
    contents <- getRecursiveFileSystem absDir
    writeFile fileName $ (unlines . filter (\x -> (last . splitOn ".") x == "gwf")) contents

extractstartGPStime :: String -> Integer
extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer

extractDataLength :: String -> Integer
extractDataLength x = read $ (!!3) $ splitOn "-" $ head $ splitOn "." $ last $ splitOn "/" x :: Integer

extractstartGPStimefromFilename :: String -> Integer
extractstartGPStimefromFilename x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer

extractDataLengthfromFilename :: String -> Integer
extractDataLengthfromFilename x = read $ (!!3) $ splitOn "-" $ head $ splitOn "." $ last $ splitOn "/" x :: Integer


