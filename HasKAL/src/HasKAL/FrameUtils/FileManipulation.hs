module HasKAL.FrameUtils.FileManipulation
( getRecursiveFileSystem
, getCurrentDirectory
, genFileList
, extractstartGPStime
, extractDataLength
, extractstartGPStimefromFilename
, extractDataLengthfromFilename
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

loadASCIIdata :: FilePath -> [[Double]]
loadASCIIdata x = unsafePerformIO $ fmap (map (map (\x->read x :: Double).words). splitLines) (readFile x)

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

