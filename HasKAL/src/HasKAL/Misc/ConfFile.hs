

module HasKAL.Misc.ConfFile
( readFileList
, readConfFile
) where

import Control.Monad (liftM)
import Data.List (isPrefixOf)

readFileList :: FilePath -> IO [String]
readFileList name = liftM (filter (\x -> head x /='#') . lines) $ readFile name

readConfFile :: FilePath -> [String] -> [String] -> IO ([String], [[String]])
readConfFile name p1 pN = do
  str <- liftM lines $ readFile name
  return $ ( map (readConfCore1 str) p1, map (readConfCoreN str) pN )

readConfCore1 :: [String] -> String -> String
readConfCore1 str l = ds
  where ds = stopHead . drop 1 . words . stopHead . filter (\x -> isPrefixOf (l++": ") x) $ str
        stopHead ss
          | length ss == 0  = error $ "Error in Conf file: can't find "++l
          | otherwise       = head ss

readConfCoreN :: [String] -> String -> [String]
readConfCoreN str l = ds
  where ds = stopZero . drop 1 . words . head . stopZero . filter (\x -> isPrefixOf (l++": ") x) $ str
        stopZero ss
          | length ss == 0  = error $ "Error in Conf file: can't find "++l
          | otherwise       = ss

