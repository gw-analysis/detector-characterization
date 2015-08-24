import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.FrameUtils.Function (readFrameV)
--import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature

import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
--import System.Environment (getArgs)
import Control.Monad (forM)

import ReadFiles

--import qualified Data.Complex as DC
--import Data.Complex (Complex( (:+) ))


--import HasKAL.TimeUtils.GPSfunction
--import HasKAL.DataBaseUtils.Function (kagraDataGet)
-- error
--zs <- kagraDataGet 1124077565 64 "K1:PEM-EX_MAG_X_FLOOR"

main = do

 {-- open frame file --}
 let channel  = "K1:PEM-EX_MAG_X_FLOOR"
     fs = 2048::Double
     ifs = 2048::Int
    
 let nfile = 100   -- you can change
     filelist = take nfile testFiles
-- let gps = 1124077533::Double
 let gps = 77533::Double

 print nfile

 maybexs <- mapM (readFrameV channel) filelist
 let xs = map (fromMaybe (error " no data in the file.")) maybexs
 let ys = DVG.concat xs
 --print $ DVG.take 100 ys
 --print $ DVG.length ys


 let iduration = 64::Int     -- you can change
     duration  = fromIntegral iduration :: Double
     totalDuration = 32.0 * (fromIntegral nfile) :: Double
     nSplit    = floor $ totalDuration / duration
 let nchunk = iduration * ifs::Int
 let tlist = [1/fs, 2/fs..duration]
     nt    = length tlist

 print nchunk
 print nSplit


 let iSplit = [0..nSplit-1]::[Int]
 rmslist <- forM iSplit $ \i -> do    -- for loop

  {-- split vector --}
  let hoff = gwpsdV (DVG.slice (nchunk*i) nchunk ys) nchunk fs
--  {-- sum each frequecy band --}
--  {-- [0.2:1] [1:4] [4:10] Hz --}
--  let f1 = 0.2
--      f2 = 1.0
  let f1 = 1.0
      f2 = 4.0

  let indx1' = DVG.findIndex (>=f1) $ fst hoff
      indx2' = DVG.findIndex (>=f2) $ fst hoff
      indx1  = fromJust indx1'
      indx2  = fromJust indx2'
  let sumHoff = DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd hoff

  -- old definition
  --let indexlist = [0..]
  -- let listSpectrum = zip (NLA.toList $ fst hoff) (NLA.toList $ snd hoff)
  -- let sumHoff = sum $ map snd $ filter ( \(x, y) -> f1<x && x<f2 ) listSpectrum

--  let fname = filelist!!(i) ++ ".png"
--  plotXV LogXY LinePoint 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 "title" ((0.001,5),(0,0)) hoff
  
  return sumHoff

--  {-- plot RMS value --}
-- plotXV Linear Dot 1 RED ("time", channel) 0.08 "RMSMon" ((0,0), (0,0)) $ DVG.zip (DVG.fromList tlist) xs


 let gpslist = [gps, gps+duration..gps+(fromIntegral nfile)*duration]::[Double]
 plotXV Linear LinePoint 1 BLUE ("GPS[sec]", "          V/s^2") 0.05 "RMSMon" ((0,0),(0,0)) (NLA.fromList gpslist, NLA.fromList rmslist)
 
 return 0



--squeezeSpectrumRange :: Spectrum -> Double -> Double -> Vector NLA.Double
--squeezeSpectrumRange spectrum f1 f2 = 

threedata2string ::[Double] -> [Double] ->[Double]-> String
threedata2string a b c = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  (map show c)

sixdata2string::[Double] -> [Double] ->[Double]->[Double]->[Double]->[Double]-> String
sixdata2string a b c d e f = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show c) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show d) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show e) $ zipWith (++) (repeat " ")  ( map show f)

