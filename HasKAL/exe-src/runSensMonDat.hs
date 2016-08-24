

-- module DailySensMon
-- ( dailySensMon
-- )
-- where
--


import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import HasKAL.DataBaseUtils.FrameFull.Function(kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.MonitorUtils.SensMon.SensMon
import HasKAL.MonitorUtils.SensMon.Signature
import HasKAL.MonitorUtils.SensMon.Data
import HasKAL.MonitorUtils.SensMon.Function
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.TimeUtils.Function(deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature
import qualified Numeric.LinearAlgebra as NL
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)


main = do
  args <- getArgs
  (fs', duration',fftsecond', ch, fname) <- case length args of
     5 -> return (args!!0, args!!1, args!!2, args!!3, args!!4)
     _ -> error "Usage: SensMon fs dur fftsec ID ColumnAsciiFile"


  datstr <- readFile fname >>= \x -> return $ lines x
  let dat = map s2d datstr

      s2d x = read x :: Double

  let fs = read fs' :: Double
      duration = read duration' :: Int
      fftsecond= read fftsecond' :: Int
      fl = 1/fromIntegral fftsecond
      fu = fs/2
      tmpname = concat $ init $ splitOn "." fname
      p = PlotParam
            { title = "SensMon: " ++ ch
            , filename = tmpname++"_"++fs'++"_"++duration'++"_"++fftsecond'++"_SensMon.png"
            , xlabel = "frequency [Hz]"
            , ylabel = "ASD [1/rHz]"
            , zlabel = "count"
            , scale = LogXZ
            , colorbar = COLZ
            }
  let datv = NL.fromList $ take (duration*floor fs) dat


  runSensMonCore datv fftsecond (ch, fs) (fl, fu) p


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


runSensMonCore :: VS.Vector Double -> Int -> (String, Double) -> (Double, Double) -> PlotParam -> IO ()
runSensMonCore dat fftsecond (ch, fs) (fl, fu) plotparam = do
  let nfft = floor fs * fftsecond
  do let (x, param) =  runSensMon dat fs nfft
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

fst':: (Num a, Num b, Num c) => (a, b, c) -> a
fst' (x, _, _) = x
snd':: (Num a, Num b, Num c) => (a, b, c) -> b
snd' (_, y, _) = y
thd':: (Num a, Num b, Num c) => (a, b, c) -> c
thd' (_, _, z) = z




