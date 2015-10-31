

module DailySensMon
( dailySensMon
)
where



import Data.Maybe (fromMaybe)
import HasKAL.DataBaseUtils.Function(kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.MonitorUtils.SensMon.SensMon
import HasKAL.MonitorUtils.SensMon.Signature
import HasKAL.MonitorUtils.SensMon.Data
import HasKAL.MonitorUtils.SensMon.Function
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.TimeUtils.Function(deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature
import System.IO.Unsafe (unsafePerformIO)



type Date = (Int, Int, Int)

dailySensMon :: Date -> String -> (Double, Double) -> (Double, Int, Double) -> IO ()
dailySensMon (year, month, day) ch (fl, fu) (hmin, ndiv, hmax) = do
  let gps = read (time2gps $ show year++"-"++show month++"-"++show day++" 00:00:00 JST") :: Int
      day = 86400
      hr = 3600
      min = 60
      gpslist = [gps, gps+hr..gps+day]
  file <- kagraDataFind (fromIntegral gps) (fromIntegral hr) ch >>= \maybef ->
    return $ head $ fromMaybe (error "file not found") maybef
  fs <- getSamplingFrequency file ch >>= \maybefs ->
          return $ fromMaybe (error "fs not loaded") maybefs
  let x = go gpslist (hr*floor fs) (ch, fs)
      fname = ch++"-"++show year++"-"++show month++"-"++show day++"_SensMon.png"
      title = "SensMon: " ++ ch
  histgram2dM LogXYZ COLZ ("frequency [Hz]", "ASD [1/rHz]", "count") title fname ((fl,fu), (hmin,hmax)) x
  where
    go (t:ts) n (ch, fs) =
      let maybev = unsafePerformIO $ kagraDataGet t n ch
       in case maybev of
            Nothing -> go ts n (ch, fs)
            Just v -> updateSensMon (runSensMon v fs (n`div`60) (hmin, ndiv, hmax))
              (go ts n (ch, fs))

