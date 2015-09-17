
import HasKAL.FrameUtils.FrameUtils
import HasKAL.FrameUtils.Function

import qualified Data.Vector.Storable as VS
import Data.List
import Data.Maybe (fromJust)
import Numeric
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils as KGL


main :: IO()
main = do
     let fname = "/home/ueno/LIGO/S6/931135488/L-L1_LOSC_4_V1-931160064-4096.gwf" ::String
     chList <- getChannelList fname
     print $ chList

--     let chname = head $ map fst (fromJust chList)
     let chname = "L1:LOSC-STRAIN" ::String
     dataVmaybe <- readFrameV chname fname

--     cnsig <- getArgs
     let nsig   = 5      :: Int
         fs     = 4096   :: Double
         order  = 6      :: Int
         fmin   = 300    :: Double
         fmax   = 400    :: Double
         nframe = 1024   :: Int
         nshift = 32     :: Int
         nstart = 0      :: Int
         nend   = 300    :: Int
         dataV = fromJust dataVmaybe 
         output = KGL.butterBandPass dataV fs fmin fmax order
     case output of
       Left message -> print message
       Right frameV_bp -> do
         let outV = KGL.nha frameV_bp fs nsig nframe nshift nstart nend
             outText = concat $ map (toText . shift) outV
         writeFile "L-L1_LOSC_4_V1-931160064-4096.ana" $ outText


toText :: [[Double]] -> String
toText xss = unlines . map (unwords . map (\x -> Numeric.showEFloat (Just 10) x "") ) . transpose $ xss 

{-
shift :: (Double, Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (tstart, tend, x, y, z) = [tstart', tend', VS.toList x, VS.toList y, VS.toList z]
  where tstart' = take num $ repeat tstart
        tend' = take num $ repeat tend
        num = VS.length x
-}

shift :: (Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (time, x, y, z) = [time', VS.toList x, VS.toList y, VS.toList z]
  where time' = take num $ repeat time
        num = VS.length x

