
--import HasKAL.FrameUtils.FrameUtils
import HasKAL.FrameUtils.Function (readFrameV)

import qualified Data.Vector.Storable as VS
import Data.List
import Data.Maybe (fromJust)
import Numeric
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils as KGL


main :: IO()
main = do
--     let gps = "1134486016" ::String
     let gps = "1144335616" ::String
     let fname = "/data/kagra/raw/full/K-K1_C-"++gps++"-32.gwf" ::String
     let chnamesub = "TRANS_DC" ::String
--     let chnamesub = "MIXER" ::String
     let chname = "K1:PSL-PMC_"++chnamesub++"_OUT_DQ" ::String
     dataVmaybe <- readFrameV chname fname     
--     cnsig <- getArgs
     let nsig   = 2      :: Int
         fs     = 2048   :: Double
         order  = 6      :: Int
         ifmin   = 50    :: Int
         ifmax   = 70    :: Int
         fmin = (fromIntegral ifmin)
         fmax = (fromIntegral ifmax)
--         nframe = 16384  :: Int
         nframe = 2048   :: Int
         nshift = 10     :: Int
         nstart = 0      :: Int
         nend   = 2000   :: Int
         t0     = 0      :: Double
         fname_out = "K1_PSL_PMC_"++chnamesub++"_"++gps++"_32_fl"
                     ++(show ifmin)++"_fh"++(show ifmax)
                     ++"_spc"++(show nsig)
                     ++"_frame"++(show nframe)++"_shift"
                     ++(show nshift)++".ana" :: String
         dataV = fromJust dataVmaybe 
         output = KGL.butterBandPass dataV fs fmin fmax order
     case output of
       Left message -> print message
       Right frameV_bp -> do
         let outV = KGL.nha frameV_bp fs nsig nframe nshift nstart nend t0
{--
             outV' = [(a,b,c,d) | (a,b,c,d) <- outV
                                , null [ e | e <- VS.toList b
                                           , isNaN e]
                                , null [ e | e <- VS.toList c
                                           , isNaN e]
                                , null [ e | e <- VS.toList d
                                           , isNaN e]]
                                     
             outText = concat $ map (toText . shift) outV'
--}
             outText = concat $ map (toText . shift) outV
--         writeFile "L-L1_LOSC_4_V1-931160064-4096.ana" $ outText
         writeFile fname_out $ outText


toText :: [[Double]] -> String
toText xss = unlines . map (unwords . map (\x -> Numeric.showEFloat (Just 10) x "") ) . transpose $ xss 

shift :: (Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (time, x, y, z) = [time', VS.toList x, VS.toList y, VS.toList z]
  where time' = take num $ repeat time
        num = VS.length x

