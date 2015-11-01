

-- module DailySensMon
-- ( dailySensMon
-- )
-- where
--



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



main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
     4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
     _ -> error "Usage: dailySensMon yyyy mm dd ch"

  let gps = read (time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST") :: Int
  file <- kagraDataFind (fromIntegral gps) (fromIntegral hr) ch >>= \maybef ->
    return $ head $ fromMaybe (error "file not found") maybef
  fs <- getSamplingFrequency file ch >>= \maybefs ->
          return $ fromMaybe (error "fs not loaded") maybefs
  let fl = 0
      fu = fs/2
  dailySensMon gps (ch, fs) (fl, fu)



dailySensMon :: Int -> (String, fs) -> (Double, Double) -> IO ()
dailySensMon gps (ch, fs) = do
  let day = 86400
      hr = 3600
      min = 60
      gpslist = [gps, gps+hr..gps+day]
  let x = go gpslist (hr*floor fs) (ch, fs)
      fname = ch++"-"++show year++"-"++show month++"-"++show day++"_SensMon.png"
      title = "SensMon: " ++ ch
      hmin = VS.minimum (snd' x)
      hmax = VS.maximum (thd' x)
  histgram2dM LogXYZ COLZ ("frequency [Hz]", "ASD [1/rHz]", "count") title fname ((fl,fu), (hmin,hmax)) x
  where
    go (t:ts) n (ch, fs) =
      let maybev = unsafePerformIO $ kagraDataGet t n ch
       in case maybev of
            Nothing -> go ts n (ch, fs)
            Just v -> updateSensMon (fst $ runSensMon v fs (n`div`60))
              (go ts n (ch, fs))


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number

fst':: (Num a, Num b, Num c) => (a, b, c) -> a
fst' (x, _, _) = x
snd':: (Num a, Num b, Num c) => (a, b, c) -> b
snd' (_, y, _) = y
thd':: (Num a, Num b, Num c) => (a, b, c) -> c
thd' (_, _, z) = z




