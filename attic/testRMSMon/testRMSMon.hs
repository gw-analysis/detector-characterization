import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature

import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Storable as V (length)
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
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
 let filelist = take 100 testFiles
 let gps = 1124077533::Double

 maybexs <- mapM (readFrameV channel) filelist
 let xs = map (fromMaybe (error " no data in the file.")) maybexs
 let ys = DVG.concat xs
 --print $ DVG.take 100 ys
 --print $ DVG.length ys

 let nfile = length filelist
 print nfile

 let duration = 32::Double
     iduration = 32::Int
 let nchunk = iduration * ifs::Int
 let tlist = [1/fs, 2/fs..32]
     nt    = length tlist

 let flist = [0.0,1.0/duration ..fs/2.0]

 {-- split vector --}
 let xs1 = DVG.slice 0 nt ys
 print $ DVG.length xs1


 {-- fft xs data --}
 let index = [0..nfile-1]::[Int]

 rmslist <- forM index $ \i -> do
  let hoff = gwpsdV (DVG.slice (10*i) (10*i+nchunk) ys) nchunk fs
--  {-- sum each frequecy band --}
--  {-- [0.2:1] [1:4] [4:10] Hz --}
--  let f1 = 0.2
--      f2 = 1.0
  let f1 = 1.0
      f2 = 4.0
  --sumHoff = DVG.sum $ DVG.filter (>f1) $ DVG.filter (<f2) hoff
  
  let indexlist = [0..]
  let listSpectrum = zip (NLA.toList $ fst hoff) (NLA.toList $ snd hoff)
  let sumHoff = sum $ map snd $ filter ( \(x, y) -> f1<x && x<f2 ) listSpectrum


--  let fname = filelist!!(i) ++ ".png"
--  plotXV LogXY LinePoint 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 "title" ((0.001,5),(0,0)) hoff
  
  return sumHoff

--  {-- plot RMS value --}
-- plotXV Linear Dot 1 RED ("time", channel) 0.08 "RMSMon" ((0,0), (0,0)) $ DVG.zip (DVG.fromList tlist) xs

  
 let gpslist = [gps, gps+duration..gps+(fromIntegral nfile)*duration]::[Double]
 print gpslist
 print rmslist
 plotXV LogY LinePoint 1 BLUE ("GPS[sec]", "?/s^2") 0.05 "title" ((0,0),(0,0)) (NLA.fromList gpslist, NLA.fromList rmslist)
 
 return 0



--squeezeSpectrumRange :: Spectrum -> Double -> Double -> Vector NLA.Double
--squeezeSpectrumRange spectrum f1 f2 = 

threedata2string ::[Double] -> [Double] ->[Double]-> String
threedata2string a b c = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  (map show c)

sixdata2string::[Double] -> [Double] ->[Double]->[Double]->[Double]->[Double]-> String
sixdata2string a b c d e f = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show c) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show d) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show e) $ zipWith (++) (repeat " ")  ( map show f)

