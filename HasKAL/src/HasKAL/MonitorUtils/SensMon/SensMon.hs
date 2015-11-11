

module HasKAL.MonitorUtils.SensMon.SensMon
( SensParam
, runSensMon
, updateSensMon
--,
) where

import qualified Data.List as DL
import qualified Data.Packed.Matrix as M
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.Function (updateMatrixElement)
import HasKAL.MonitorUtils.SensMon.Signature
import HasKAL.MonitorUtils.SensMon.Data
import HasKAL.MonitorUtils.SensMon.Function
import HasKAL.TimeUtils.Function(deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature



runSensMon :: VS.Vector Double -> Double -> Int -> (SensSpectrum, SensParam)
runSensMon input fs m =
  let param = SensParam
        { histmax = 0
        , histmin = 0
        , ndiv = 50
        , binInterval = 0
        , binlist = []
        }
   in runSensMonCore input fs m param


updateSensMon :: SensSpectrum -> SensSpectrum -> SensSpectrum
updateSensMon history new =
  let (_, _, historyM) = history
      (_, _, newM)     = new
      updatedM = updateMatrixElement historyM mindxs values
      mindxs = [(c, r) | c<-[0..rows historyM-1], r<-[0..cols historyM-1]]
      values = [historyM @@> (x, y) + newM @@> (x, y) | x<-[0..rows historyM-1], y<-[0..cols historyM-1]]
   in updateSensMonInternal history updatedM


runSensMonCore :: VS.Vector Double -> Double -> Int -> SensParam -> (SensSpectrum, SensParam)
runSensMonCore input fs n param' =
  let (chunks, param) = setHistParam input n fs param'
      n2 = n `div` 2
      eachFbin = M.toColumns . M.fromRows $ chunks
      hmax = histmax param
      hmin = histmin param
      bins = binlist param
   in (( fromList [fs*fromIntegral i/fromIntegral n|i<-[0..n2]]
      , VS.fromList $ map (logBase 10) (init bins)
      , M.fromColumns
        $ map (VS.fromList . snd . histogram1d hmin hmax bins . VS.toList) eachFbin)
      , param)


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


setHistParam :: VS.Vector Double -> Int -> Double -> SensParam -> ([VS.Vector Double], SensParam)
setHistParam  dat nfft fs param =
  let chunks = mkChunks dat nfft
      vlist = map (\x -> sqrt . snd $ gwOnesidedPSDV x nfft fs) chunks
--      hmax = DL.maximum $ map VS.maximum vlist
--      hmin = DL.minimum $ map VS.minimum vlist
      sdat = VS.modify I.sort $ VS.concat vlist
      nv = VS.length sdat
      hmin = sdat VS.! floor (0.05*fromIntegral nv)
-- --      hmax = sdat VS.! (nv - floor (0.001*fromIntegral nv))
      hmax = sdat VS.! (nv -1)
      binInterval' = (logBase 10 hmax - logBase 10 hmin)/fromIntegral (ndiv param)
      binlist' = map (10**) [logBase 10 hmin, logBase 10 hmin+binInterval' ..logBase 10 hmax]
      param1 = updateSensParam'histmin param hmin
      param2 = updateSensParam'histmax param1 hmax
      param3 = updateSensParam'binInterval param2 binInterval'
      param4 = updateSensParam'binlist param3 binlist'
   in (vlist, param4)


