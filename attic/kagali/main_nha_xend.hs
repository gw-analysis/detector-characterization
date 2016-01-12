
--import HasKAL.FrameUtils.FrameUtils
import HasKAL.FrameUtils.Function (readFrameV)

import qualified Data.Vector.Storable as VS
import Data.List
import Data.Maybe (fromJust)
import Numeric
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils as KGL


main :: IO()
main = do
--     let dir = "R0206" ::String
     let dir = "R0207" ::String
--     let gps = "1120643968" ::String
--     let gps = "1121096127" ::String
--     let gps = "1121096191" ::String
     let gps = "1121096543" ::String
--     let fname = "/home/ueno/LIGO/S6/931135488/L-L1_LOSC_4_V1-931160064-4096.gwf" ::String
     let fname = "/data/kagra/xend/"++dir++"/K-K1_R-"++gps++"-32.gwf" ::String
--     chList <- getChannelList fname
--     print $ chList
--     let chname = head $ map fst (fromJust chList)
--     let chname = "L1:LOSC-STRAIN" ::String

     let chnamesub = "ACC_NO2_X_FLOOR" ::String
--     let chnamesub = "ACC_NO2_Y_FLOOR" ::String
--     let chnamesub = "ACC_NO2_Z_FLOOR" ::String
     let chname = "K1:PEM-EX_"++chnamesub ::String
     dataVmaybe <- readFrameV chname fname
     
--     cnsig <- getArgs
     let nsig   = 2      :: Int
         fs     = 2048   :: Double
         order  = 6      :: Int
{-
         fmin   = 200    :: Double
         fmax   = 250    :: Double
-}
         ifmin   = 50    :: Int
         ifmax   = 70    :: Int
         fmin = (fromIntegral ifmin)
         fmax = (fromIntegral ifmax)
         nframe = 2048   :: Int
--         nshift = 512    :: Int
         nshift = 1    :: Int
         nstart = 0      :: Int
--         nend   = 100000 :: Int
         nend   = 2000 :: Int
         t0     = 0      :: Double
         fname_out = "K_K1_R_"++chnamesub++"_"++gps++"_32_fl"
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

