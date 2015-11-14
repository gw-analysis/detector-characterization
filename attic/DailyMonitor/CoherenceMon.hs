
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV)
import HasKAL.MonitorUtils.CoherenceMon.Function
import HasKAL.PlotUtils.HROOT.PlotGraph

main = do
  args <- getArgs
  (year, month, day, ch1, ch2) <- case length args of
                              5 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3, args!!4)
                              _ -> error "Usage: CoherenceMon yyyy mm dd ch1 ch2"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for CoherenceMon
      fftLength = 64   -- seconds
      -- for Plot
      oFile = ch1++"_"++ch2++"-"++year++"-"++month++"-"++day++"_CoherenceMon.png"
      title = "CoherenceMon: " ++ ch1 ++ " vs " ++ ch2
      xlabel = "Date: "++year++"/"++month++"/"++day

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch1
  let file = case mbFiles of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust mbFiles
  mbDat1 <- kagraDataGet gps duration ch1
  mbFs1 <- getSamplingFrequency file ch1
  mbDat2 <- kagraDataGet gps duration ch2
  mbFs2 <- getSamplingFrequency file ch2
  let (dat1, fs1, dat2, fs2) = case (mbDat1, mbFs1, mbDat2, mbFs2) of
                   (Just a, Just b, Just c, Just d) -> (a, b, c, d)
                   (Nothing, _, _, _) -> error $ "Can't read data: "++ch1++"-"++year++"/"++month++"/"++day
                   (_, Nothing, _, _) -> error $ "Can't read sampling frequency: "++ch1++"-"++year++"/"++month++"/"++day
                   (_, _, Nothing, _) -> error $ "Can't read data: "++ch2++"-"++year++"/"++month++"/"++day
                   (_, _, _, Nothing) -> error $ "Can't read sampling frequency: "++ch2++"-"++year++"/"++month++"/"++day

  {-- main --}
  let coh = coherenceMon fftLength fs1 fs2 dat1 dat2
  plotV Linear Line 1 BLUE (xlabel, "|coh(f)|^2") 0.05 title oFile ((0,0),((-0.05),1.05)) coh

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
