

import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon (studentRayleighMonWaveData)
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SpectrumUtils.Function (catSpectrogramT0)
import HasKAL.TimeUtils.Function (diffGPS, deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (getMaximumChunck)

import qualified Data.Vector.Storable as V

main = do
  args <- getArgs
  (year, month, day, hour, minute, second, duration', chunk', fftsec', dt, df, ch) <- case length args of
   12 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), show0 2 (args!!3), show0 2 (args!!4), show0 2 (args!!5), args!!6, args!!7, args!!8, args!!9, args!!10, args!!11)
   _ -> error "Usage: SRMon yyyy mm dd hh mm ss duration[s] chunklen[s] fftsec[s] dt[s] df[Hz] channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" JST") :: Int
      duration = read duration' :: Double -- seconds
      -- for SRMon
      fftLength = read fftsec' :: Double    -- seconds
      srmLength = read chunk :: Double -- seconds
      timeShift = read dt :: Double -- seconds
      freqResol = read df :: Double   -- Hz
      quantile  = 0.99 -- 0 < quantile < 1
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"-"++hour++"-"++minute++"-"++second++"JST"++"_SRMon.png"
      title = "StudentRayleighMon: " ++ ch ++ " (dt= "++(show srmLength)++"s, df= "++(show freqResol)++"Hz)"
--      xlabel = "Date: "++year++"/"++month
      xlabel = "Time[s] since "++year++"/"++month++"/"++day++":"++hour++":"++minute++":"++second++"JST"

  {-- read data --}
  mbWd <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch
  let wd = case mbWd of
            Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
            Just x -> x

  {-- main --}
  let nu = map (studentRayleighMonWaveData quantile fftLength srmLength timeShift freqResol (getMaximumChunck wd)) wd
      n0 = nblocks timeShift gps duration wd
--  histgram2dDateM Linear COLZ (xlabel, "frequency [Hz]", "nu") title oFile ((0,0),(0,0)) gps $ catSpectrogramT0 0 timeShift n0 nu
  histgram2dM Linear COLZ (xlabel, "frequency [Hz]", "nu") title oFile ((0,0),(0,0)) $ toHours $ catSpectrogramT0 0 timeShift n0 nu


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

nblocks :: Double -> Int -> Int -> [WaveData] -> [Int]
nblocks dt gps duration [] = [ceiling . (/dt) . fromIntegral $ duration]
nblocks dt gps duration ws = map (ceiling . (/dt) . deformatGPS . uncurry diffGPS) ss
  where ss = (startGPSTime $ head ws, (gps,0))
             : map (\i -> (startGPSTime $ ws!!i, stopGPSTime $ ws !!(i-1)) ) [1..length ws -1]
             ++ [((gps+duration, 0), stopGPSTime $ last ws)]

toHours spe = (V.map (1*) t, f, m)
  where (t,f,m) = spe

