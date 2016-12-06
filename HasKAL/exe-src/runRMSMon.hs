import qualified Data.Vector.Generic as DVG
import System.Environment (getArgs)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMonWaveData)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGet0)
--import HasKAL.DataBaseUtils.XEndEnv.Function (kagraWaveDataGet0)
import HasKAL.WaveUtils.Data (WaveData(..))
import qualified Data.Vector.Storable as V

{-- memo
    running time (24hour data) : ~2m30s
--}

-- TODO : how we get unit of y-axis?

main = do
 args <- getArgs
 (year, month, day, hour, minute, second, duration', fftsec', channel, f1low, f1high, f2low, f2high, f3low, f3high) <- case length args of
     15 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), show0 2 (args!!3), show0 2 (args!!4), show0 2 (args!!5), args!!6, args!!7, args!!8, args!!9, args!!10, args!!11, args!!12, args!!13, args!!14)
     _ ->  error "Usage: RMSMon yyyy mm dd hh mm ss duration fftsec channel f1low f1high f2low f2high f3low f3high"


 {-- parameters --}
 let gps = read (time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" JST") :: Int
     totalduration = read duration' :: Int -- 1day = 86400s
 {-- for RMSMon --}
 let f1band = ((read f1low::Double), (read f1high::Double))
     f2band = ((read f2low::Double), (read f2high::Double))
     f3band = ((read f3low::Double), (read f3high::Double))
     freq  = [f1band, f2band, f3band]::[(Double, Double)]
     fftsec = read fftsec' :: Double

 maybewd <- kagraWaveDataGet0 (fromIntegral gps) (fromIntegral totalduration) channel
 let wd = case maybewd of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              Just x -> x

 print $ DVG.take 100 (gwdata wd)
 print $ DVG.length (gwdata wd)
 print $ samplingFrequency wd
 print $ startGPSTime wd
 print $ stopGPSTime wd
 {-- for RMSMon --}
 let nmon = floor (fftsec * (samplingFrequency wd)) ::Int -- 86400s / 96[chunk] = 900[s]
 let rms  = rmsMonWaveData nmon freq wd
     rms_max = DVG.maximum $ DVG.concat (map snd rms)
     rms_min = DVG.minimum $ DVG.concat (map snd rms)

 let color = [BLUE, GREEN, RED, PINK, CYAN]
     title = setTitle f1low f1high f2low f2high f3low f3high channel
     fname = channel ++ "-" ++ year ++ "-" ++ month ++ "-" ++ day ++":"++hour++":"++minute++":"++second++"JST"++"_RMSMon.png"
--     xlabel = "Date: "++year++"/"++month :: String
     xlabel = "Time[s] since "++year++"/"++month++"/"++day++":"++hour++":"++minute++":"++second++"JST"++"_"++duration'

-- oPlotDateV LogY [LinePoint] 1 color (xlabel, "RMS") 0.05 title fname ((0,0),(rms_min*0.8,rms_max*1.2)) (fst $ startGPSTime wd) rms
-- oPlotDateV LogY [LinePoint] 1 color (xlabel, "RMS") 0.05 title fname ((0,0),(0,0)) (fst $ startGPSTime wd) rms
 oPlotV LogY [LinePoint] 1 color (xlabel, "RMS") 0.05 title fname ((0,0),(0,0)) $ map toHours rms

 return 0

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

setTitle :: String -> String -> String -> String -> String -> String -> String -> String
setTitle f1low f1high f2low f2high f3low f3high channel = 
         "RMS Mon BLUE=" ++ f1low ++ "-" ++ f1high ++ "Hz, GREEN=" ++ f2low ++ "-" ++ f2high ++ "Hz, RED=" ++ 
             f3low ++ "-" ++ f3high ++ ":" ++ channel

toHours x = (t,val)
 where (t',val) = x
       t = V.map (1*) t'
