

-- module DailySensMon
-- ( dailySensMon
-- )
-- where
--



import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
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
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)


main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
     4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
     _ -> error "Usage: SensMon yyyy mm dd ch"

  let gps = read (time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST") :: Int
  file <- kagraDataFind (fromIntegral gps) 3600 ch >>= \maybef ->
    return $ head $ fromMaybe (error "file not found") maybef
  fs <- getSamplingFrequency file ch >>= \maybefs ->
          return $ fromMaybe (error "fs not loaded") maybefs
  let fl = 0
      fu = fs/2
      p = PlotParam
            { title = "SensMon: " ++ ch
            , filename = ch++"-"++year++"-"++month++"-"++day++"_SensMon.png"
            , xlabel = "frequency [Hz]"
            , ylabel = "ASD [1/rHz]"
            , zlabel = "count"
            , scale = LogXZ
            , colorbar = COLZ
            }

  dailySensMonCore gps (ch, fs) (fl, fu) p


data PlotParam = PlotParam
       { title :: String
       , filename :: String
       , xlabel :: String
       , ylabel :: String
       , zlabel :: String
       , scale :: LogOption
       , colorbar :: PlotTypeOption3D
       }

updatePlotParam'title p x = p {title = x}
updatePlotParam'filename p x = p {filename = x}
updatePlotParam'xlabel p x = p {xlabel = x}
updatePlotParam'ylabel p x = p {ylabel = x}
updatePlotParam'scale p x = p {scale = x}
updatePlotParam'colorbar p x = p {colorbar = x}


dailySensMonCore :: Int -> (String, Double) -> (Double, Double) -> PlotParam -> IO ()
dailySensMonCore gps (ch, fs) (fl, fu) plotparam = do
  let day = 86400
      hr = 3600
      x = 120
      nfft = floor fs * x
      maybev = unsafePerformIO $ kagraDataGet gps day ch
   in case maybev of
        Nothing -> return ()
        Just v ->
          do let (x, param) =  runSensMon v fs nfft
                 hmin = histmin param
                 hmax = histmax param
                 title' = title plotparam
                 fname' = filename plotparam
                 xlabel' = xlabel plotparam
                 ylabel' = ylabel plotparam
                 zlabel' = zlabel plotparam
                 scale' = scale plotparam
                 color' = colorbar plotparam
             histgram2dM scale' color' (xlabel', ylabel', zlabel') title' fname' ((fl,fu), (hmin,hmax+1)) x


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

fst':: (Num a, Num b, Num c) => (a, b, c) -> a
fst' (x, _, _) = x
snd':: (Num a, Num b, Num c) => (a, b, c) -> b
snd' (_, y, _) = y
thd':: (Num a, Num b, Num c) => (a, b, c) -> c
thd' (_, _, z) = z




