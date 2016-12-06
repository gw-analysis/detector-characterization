
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.MonitorUtils.CoherenceMon.Function (coherenceMonW)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (getMaximumChunck, getCoincidentData)

main = do
  args <- getArgs
  (year, month, day, hour, minute, second, duration', fftsec', ch1, ch2) <- case length args of
    10 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), show0 2 (args!!3), show0 2 (args!!4), show0 2 (args!!5), args!!7, args!!8, args!!9, args!!10)
    _ -> error "Usage: CoherenceMon yyyy mm dd hh mm ss duration fftsec ch1 ch2"

  {-- parameters --}
  let gps = read (time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" JST") :: Int
      duration = read duration' :: Int -- seconds
      -- for CoherenceMon
      fftLength = read fftsec' :: Double   -- seconds
      -- for Plot
      oFile = ch1++"_"++ch2++"-"++year++"-"++month++"-"++day++":"++hour++":"++minute++":"++second++"JST"++"_CoherenceMon.png"
      title = "CoherenceMon: " ++ ch1 ++ " vs " ++ ch2
      xlabel = "Date: "++year++"/"++month++"/"++day++":"++hour++":"++minute++":"++second++"JST"++"_"++duration'

  {-- read data --}
  [mbWd1, mbWd2] <- mapM (kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration)) [ch1, ch2]
  let [ws1, ws2] = case (mbWd1, mbWd2) of
                    (Just x, Just y) -> getCoincidentData [x, y]
                    (Nothing, _) -> error $ "Can't find data: "++ch1
                    (_, Nothing) -> error $ "Can't find data: "++ch2


  {-- main --}
  case ws1 /= [] of
   True -> do
     let [wd1, wd2] = map getMaximumChunck [ws1, ws2]
         coh = coherenceMonW fftLength wd1 wd2
     plotV Linear Line 1 BLUE (xlabel, "|coh(f)|^2") 0.05 title oFile ((0,0),((-0.05),1.05)) coh
   False ->
     error $ "Channels are not active at the same time.\n   "++ch1++"\n   "++ch2

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
