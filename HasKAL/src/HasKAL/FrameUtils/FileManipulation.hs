module HasKAL.FrameUtils.FileManipulation
( getRecursiveFileSystem
, getCurrentDirectory
, genFileList
, extractstartGPStime
, loadASCIIdata
, loadASCIIdataM
, loadASCIIdataCV
, loadASCIIdataRV
, splitLines
, fixLines
) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import System.FilePath ((</>))
import Data.List.Split (splitOn)
import Numeric.LinearAlgebra


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

loadASCIIdata :: FilePath -> [[Double]]
loadASCIIdata x = fmap (map ((map (\x->read x :: Double)).words)) $ fmap splitLines $ readFile

loadASCIIdataM :: FilePath -> Matrix Double
loadASCIIdataM x = unsafePerformIO $ do
  rc <- fileDimensions x
  fromFile x rc

loadASCIIdataCV :: FilePath -> [Vector Double]
loadASCIIdataCV x = unsafePerformIO $ do
  rc <- fileDimensions x
  m <- fromFile x rc
  return $ toColumns m

loadASCIIdataRV :: FilePath -> [Vector Double]
loadASCIIdataRV x = unsafePerformIO $ do
  rc <- fileDimensions x
  m <- fromFile x rc
  return $ toRows m

-- file: ch04/SplitLines.hs
splitLines :: String -> [String]

-- file: ch04/SplitLines.hs
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

-- file: ch04/SplitLines.hs
fixLines :: String -> String
fixLines input = unlines (splitLines input)

