
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector)

import qualified Data.Vector.Storable as VS
import Data.List
import Numeric

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
--import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGet, kagraDataFind)
import qualified HasKAL.DataBaseUtils.FrameFull.Function as FF
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV, gwspectrogramV)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils

main = do
  args <- getArgs
  (year, month, day, hour, minute, second, duration',nframe', fcenter, ch) <- case length args of
   10 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), show0 2 (args!!3), show0 2 (args!!4), show0 2 (args!!5), args!!6, args!!7, args!!8, args!!9)
   _ -> error "Usage: LT yyyy mm dd channel"
                   
  {-- parameters --}
  let gps = read (time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" JST") :: Int
      duration = read duration' :: Int -- seconds
--      duration = 2048 -- seconds
      duration_half =  duration `div` 2 -- seconds
      gps1 = read (time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" JST") :: Int
      gps2 = gps1 + duration_half
      order    = 6      :: Int
--      nsig     = 2      :: Int
      nsig     = 1      :: Int
      nframe   = read nframe' :: Int
      fcenter1 = read fcenter :: Double
      fcenter2 = 120    :: Double
      fcenter3 = 550    :: Double
      quantiles  = [0.50, 0.95, 0.99] -- 0 < quantile < 1
      -- for Plot
      oFile0 = ch++"-"++year++"-"++month++"-"++day++"-"++hour++"-"++minute++"-"++second++"JST"++"_LTA.png"
      oFile1 = ch++"-"++year++"-"++month++"-"++day++"-"++hour++"-"++minute++"-"++second++"JST"++"_LTF.png"
      title = "LineTracker(RED=1st, BLUE=2nd): " ++ ch
      xlabel = "Date: "++year++"/"++month
--      xlabel = "time [sec] at "++year++"/"++month++"/"++day
  
    
  {-- read data --}
  mbFiles1 <- FF.kagraDataFind (fromIntegral gps1) (fromIntegral duration_half) ch
  let file1 = case mbFiles1 of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust mbFiles1
  mbDat1 <- FF.kagraDataGet gps1 duration_half ch
  
  mbFiles2 <- FF.kagraDataFind (fromIntegral gps2) (fromIntegral duration_half) ch
  let file2 = case mbFiles2 of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust mbFiles2
  mbDat2 <- FF.kagraDataGet gps2 duration_half ch
  
  mbFs <- getSamplingFrequency file1 ch
  
  let (dat1, fs) = case (mbDat1, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day
                   
  let (dat2, fs) = case (mbDat2, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day
                   
  {-- main --}
  let nshift  = (truncate fs*duration_half) `div` 128
      nstart  = 0
      nend    = (truncate fs*duration_half)
      t01     = 0
      t02     = (fromIntegral duration_half)
      output1  = butterBandPass dat1 fs (fcenter1*0.8) (fcenter1*1.2) order
      output2  = butterBandPass dat2 fs (fcenter1*0.8) (fcenter1*1.2) order
  result1 <- case output1 of
    Left message -> error message
    Right datBP1 -> do
      return $ formatNHA $ nha datBP1 fs nsig nframe nshift nstart nend t01
  result2 <- case output2 of
    Left message -> error message
    Right datBP2 -> do
      return $ formatNHA $ nha datBP2 fs nsig nframe nshift nstart nend t02
      
  let result = zipWith (zipWith concat2) result1 result2
{-- 
  oPlotV Linear LinePoint 1 [RED, BLUE]
  oPlotV Linear LinePoint 1 [RED, BLUE]
  oPlotV Linear Point 1 [RED, BLUE]
         (xlabel, "Amplitude") 0.05 title oFile0 ((0,0),(0,0)) (result!!0)
  oPlotV Linear Point 1 [RED, BLUE]
         (xlabel, "Frequency [Hz]") 0.05 title oFile1 ((0,0),((fcenter1*0.9),(fcenter1*1.1))) (result!!1)
 --}
  oPlotDateV Linear [Point, Point] 1 [RED,BLUE] 
             (xlabel, "Ampltitude") 0.05 title oFile0 ((0,0),(0,0)) gps (result!!0)
  oPlotDateV Linear [Point, Point] 1 [RED,BLUE] 
             (xlabel, "Frequency [Hz]") 0.05 title oFile1 ((0,0),((fcenter1*0.9),(fcenter1*1.1))) gps (result!!1)

{--
  case output1 of
    Left message -> print message
    Right datBP1 -> do
      let outV1 = nha datBP1 fs nsig nframe nshift nstart nend t01
          outText1 = concat $ map (toText . shift) outV1
      writeFile "LineTracking1.ana" $ outText1
  case output2 of
    Left message -> print message
    Right datBP2 -> do
      let outV2 = nha datBP2 fs nsig nframe nshift nstart nend t02
          outText2 = concat $ map (toText . shift) outV2
      writeFile "LineTracking2.ana" $ outText2
--}


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

toText :: [[Double]] -> String
toText xss = unlines . map (unwords . map (\x -> Numeric.showEFloat (Just 10) x "") ) . transpose $ xss 

shift :: (Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (time, x, y, z) = [time', VS.toList x, VS.toList y, VS.toList z]
  where time' = take num $ repeat time
        num = VS.length x

concat2 :: (VS.Vector Double, VS.Vector Double) -> (VS.Vector Double, VS.Vector Double) -> (VS.Vector Double, VS.Vector Double)
concat2 (x, y) (z, w) = (VS.concat[x, z], VS.concat[y, w])
