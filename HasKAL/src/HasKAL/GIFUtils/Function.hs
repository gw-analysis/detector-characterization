

module HasKAL.GIFUtils.Function
( loadGIFdata
, loadGIFdataV
)
where

import qualified Data.Vector.Storable as V
import System.Process
import System.IO.Unsafe (unsafePerformIO)

type OPT = String

-- | Load GIF binary data as a list
-- |
loadGIFdata :: OPT       -- ^ option for od, fD or fF
            -> FilePath  -- ^ GIF data file
            -> [Double]  -- ^  output list
loadGIFdata option fpath
  | option `elem` ["fD","fF"] =
      let s = unsafePerformIO $ readProcess "od" ["-t", option, fpath] []
       in map (\x->read x :: Double) $ concatMap (drop 1) $ filter (\x ->length x == 3) $ map words (lines s)
  | otherwise = error errmsg


-- | Load GIF binary data as a vector
-- |
loadGIFdataV :: OPT             -- ^ option for od, fD or fF
             -> FilePath        -- ^ GIF data file
             -> V.Vector Double -- ^ output vector
loadGIFdataV option fpath
  | option `elem` ["fD","fF"] =
      let s = unsafePerformIO $ readProcess "od" ["-t", option, fpath] []
       in V.fromList $ map (\x->read x :: Double) $ concatMap (drop 1) $ filter (\x ->length x == 3) $ map words (lines s)
  | otherwise = error errmsg


errmsg =  "Usage: loadGIFdata fD(fF) FilePath"
