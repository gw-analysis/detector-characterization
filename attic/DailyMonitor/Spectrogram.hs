
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector)
import Numeric (showGFloat)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getUnitY)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramV)
import HasKAL.SpectrumUtils.Function (mapSpectrogram)
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SignalProcessingUtils.Resampling (downsampleV)

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: Spectrogram yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      dsfs = 2048
      -- for Spectrogram
      fftLength = 120    -- seconds
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_Spectrogram.png"
      title = "Spectrogram " ++"(df="++(showGFloat (Just 4) (1/fftLength) "")++"): " ++ ch
      xlabel = "Date: "++year++"/"++month

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
  mbUnit <- getUnitY file ch
  let unit = case mbUnit of
              Just x  -> "["++x++"/rHz]"
              Nothing -> "[/rHz]"

  {-- main --}
  -- let hf  = gwspectrogramV 0 (truncate $ fftLength * fs) fs dat
  -- histgram2dDateM LogYZ COLZ (xlabel, "frequency [Hz]", unit) title oFile ((0,0),(0,0)) gps $ mapSpectrogram sqrt hf
  let (minfs, dat') = case (fs > dsfs) of
                       True -> (dsfs, downsampleV fs dsfs dat)
                       False -> (fs, dat)
  let hf  = gwspectrogramV 0 (truncate $ fftLength * minfs) minfs dat'
  histgram2dDateM LogYZ COLZ (xlabel, "frequency [Hz]", unit) title oFile ((0,0),(0,0)) gps $ mapSpectrogram sqrt hf
  


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

