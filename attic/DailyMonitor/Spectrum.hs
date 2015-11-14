
import System.Environment (getArgs)
import Data.Packed.Vector (subVector)
import Numeric (showGFloat)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getUnitY)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV)
import HasKAL.SpectrumUtils.Function (mapSpectrum)
import HasKAL.PlotUtils.HROOT.PlotGraph


main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: Spectrum yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for Spectrum
      fftLength = 120  -- seconds
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_Spectrum.png"
      title = "Spectrum " ++"(df="++(showGFloat (Just 4) (1/fftLength) "")++"): " ++ ch
      xlabel = "frequency [Hz] at "++year++"/"++month++"/"++day

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let file = case mbFiles of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              Just x -> head $ x
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
  let snf = gwOnesidedPSDV dat (truncate $ fftLength * fs) fs
  plotV LogXY Line 1 RED (xlabel, unit) 0.05 title oFile ((0,0),(0,0)) $ mapSpectrum sqrt snf


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

