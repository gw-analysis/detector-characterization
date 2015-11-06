
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV, gwspectrogramV)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: dailyRMon yyyy mm dd channel"
                             
  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
{--      duration = 86400 -- seconds --}
{--      duration = 2048 -- seconds --}
      duration = 86400 -- seconds
      order  = 6      :: Int
      nsig   = 2      :: Int
      nframe = 2048   :: Int
      nstart = 0      :: Int
      fmin   = 300    :: Double
      fmax   = 400    :: Double
      quantiles  = [0.50, 0.95, 0.99] -- 0 < quantile < 1
      -- for Plot
      oFile0 = ch++"-"++year++"-"++month++"-"++day++"_amp_dailyLT.png"
      oFile1 = ch++"-"++year++"-"++month++"-"++day++"_freq_dailyLT.png"
      title = "RayleighMon(RED=0.5, BLUE=0.95, PINK=0.99): " ++ ch
      xlabel = "time [sec] at "++year++"/"++month++"/"++day

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let file = case mbFiles of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust mbFiles
  mbDat <- kagraDataGet gps duration ch
  mbFs <- getSamplingFrequency file ch
  let (dat, fs) = case (mbDat, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day
                   
  {-- main --}
  let nend = (truncate fs*duration)
{--      nshift = nend `div` 2048 --}
{--      nshift = nend `div` 16 --}
      nshift = nend `div` 2048
      output = butterBandPass dat fs fmin fmax order
  case output of
    Left message -> print message
    Right datBP -> do
      let result = formatNHA $ nha datBP fs nsig nframe nshift nstart nend
{--      oPlotV Linear LinePoint 1 [RED, BLUE] --}
{--      oPlotV Linear LinePoint 1 [RED, BLUE] --}
      oPlotV Linear Point 1 [RED, BLUE]
          (xlabel, "Amplitude") 0.05 title oFile0 ((0,0),(0,0)) (result!!0)
      oPlotV Linear Point 1 [RED, BLUE]
          (xlabel, "Frequency [Hz]") 0.05 title oFile1 ((0,0),(fmin,fmax)) (result!!1)

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
