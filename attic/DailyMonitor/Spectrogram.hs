
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramV)
import HasKAL.SpectrumUtils.Function (mapSpectrogram)
import HasKAL.PlotUtils.HROOT.PlotGraph3D


main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: dailySpectrogram yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for Spectrogram
      fftLength = 1    -- seconds
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_dailySpectrogram.png"
      title = "Spectrogram: " ++ ch
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

  {-- main --}
  let hf  = gwspectrogramV 0 (truncate $ fftLength * fs) fs dat
  histgram2dDateM LogZ COLZ (xlabel, "frequency [Hz]", "[x/rHz]") title oFile ((0,0),(0,0)) gps $ mapSpectrogram sqrt hf


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

