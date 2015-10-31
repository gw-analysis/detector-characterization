

module HasKAL.MonitorUtils.SensMon.SensMon
( SensParam
, runSensMon
, updateSensMon
--,
) where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import qualified Data.Packed.Matrix as M
import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
import HasKAL.DataBaseUtils.Function (kagraDataGet)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.Function (updateMatrixElement)
import HasKAL.MonitorUtils.SensMon.Signature
import HasKAL.MonitorUtils.SensMon.Data
import HasKAL.MonitorUtils.SensMon.Function
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.TimeUtils.Function(deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature
import System.IO.Unsafe (unsafePerformIO)


type Date = (Int, Int, Int)

dailySensMon :: Date -> String -> (Double, Double) -> (Double, Double) -> IO ()
dailySensMon (year, month, day) ch (fl, fu) (hmin, hmax) = do
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      day = 86400
      hr = 3600
      min = 60
      gpslist = [gps, gps+hr..gps+day]
      file = head $ fromMaybe (error "file not found")
               $ kagraDataFind (fromIntegral gps) (fromIntegral hr) ch
  fs <- getSamplingFequency file ch >>= \maybefs ->
          return $ fromMaybe (error "fs not loaded") maybefs
  let x = go gpslist ch
      fname = ch++"-"++year++"-"++month++"-"++day++"_SensMon.png"
      title = "SensMon: " ++ ch
  histgram2dM LogXYZ COLZ ("frequency [Hz]", "ASD [1/rHz]", "count") title fname ((fl,fu), (hmin,hmax)) x
  where
    go (t:ts) ch =
      let maybev = unsafePerformIO $ kagraDataGet t hr ch
       in case maybev of
            Nothing -> go ts ch
            Just v -> updateSensMon (runSensMon v fs (min*fs) (hmin, min*fs, hmax)) (go ts ch)


runSensMon :: VS.Vector Double -> Double -> Int -> (Double, Int, Double) -> SensSpectrum
runSensMon input fs n (hmin, n, hmax) =
  let param = SensParam
        { histmax = hmax
        , histmin = hmin
        , ndiv = n
        , binInterval = (logBase 10 (histmax param) - logBase 10 (histmin param))/fromIntegral (ndiv param)
        , binlist = map (10**) [logBase 10 (histmin param), logBase 10 (histmin param)
            +binInterval param ..logBase 10 (histmax param)]
        }
   in runSensMonCore input fs n param


updateSensMon :: SensSpectrum -> SensSpectrum -> SensSpectrum
updateSensMon history new =
  let (_, _, historyM) = history
      (_, _, newM)     = new
      updatedM = updateMatrixElement historyM mindxs values
      mindxs = [(c, r) | c<-[0..rows historyM-1], r<-[0..cols historyM-1]]
      values = [historyM @@> (x, y) + newM @@> (x, y) | x<-[0..rows historyM-1], y<-[0..cols historyM-1]]
   in updateSensMonInternal history updatedM


runSensMonCore :: VS.Vector Double -> Double -> Int -> SensParam -> SensSpectrum
runSensMonCore input fs n param =
  let chunks = mkChunks input n
      n2 = n `div` 2
      vlist  = map (\x -> snd $ gwOnesidedPSDV x n fs) chunks
      eachFbin = M.toColumns . M.fromRows $ vlist
      hmax = histmax param
      hmin = histmin param
      bins = binlist param
   in ( fromList [fs*fromIntegral i/fromIntegral nfft|i<-[0..n2]]
      , VS.fromList $ init bins
      , M.fromColumns
        $ map (VS.fromList . snd . histogram1d hmin hmax bins . VS.toList) eachFbin)


mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (VS.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = VS.slice 0 n vIn :  mkChunksCore (VS.drop n vIn) n (m-1)


histogram1d :: Double -> Double -> [Double] -> [Double] -> ([Double], [Double])
histogram1d xmin xmax bins input =
  let intervals = zipWith (\x y ->(x, y)) (init bins) (tail bins)
      within u x = x >= fst u && x < snd u
   in (map fst intervals, map ((fromIntegral.length) . (\u -> filter (within u) input)) intervals)




