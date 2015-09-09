

module HasKAL.MonitorUtils.SensMon
( SensParam
, runSensMon
, updateSensMon
--,
) where

import qualified Data.Packed.Matrix as M
import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.Function (updateMatrixElement)
import Signature
import Data
import Function


runSensMon :: VS.Vector Double -> Double -> Int -> SensSpectrum
runSensMon input fs n =
  let param = SensParam
        { histmax = 1.0E-18
        , histmin = 1.0E-24
        , binInterval = (logBase 10 (histmax param) - logBase 10 (histmin param))/100.0
        , binlist = map (10**) [logBase 10 (histmin param), logBase 10 (histmin param)
            +(binInterval param) ..logBase 10 (histmax param)]
        }
   in runSensMonCore input fs n param


updateSensMon :: SensSpectrum -> SensSpectrum -> SensSpectrum
updateSensMon history new = do
  let (_, _, historyM) = history
      (_, _, newM)     = new
      updatedM = updateMatrixElement historyM mindxs values
      mindxs = [(c, r) | c<-[0..rows historyM-1], r<-[0..cols historyM-1]]
      values = [historyM @@> (x, y) + newM @@> (x, y) | x<-[0..rows historyM-1], y<-[0..cols historyM-1]]
   in updateSensMonInternal history updatedM


runSensMonCore :: VS.Vector Double -> Double -> Int -> SensParam -> SensSpectrum
runSensMonCore input fs n param = do
  let chunks = mkChunks input n
      n2 = n `div` 2
      vlist  = map (\x -> VS.take n2
        $ VS.map (sqrt . (*2)) $ snd $ gwpsdV x n fs) chunks
      eachFbin = M.toColumns . M.fromRows $ vlist
      hmax = histmax param
      hmin = histmin param
      bins = binlist param
   in ( VS.take n2 $ linspace n (0, fs)
      , VS.fromList bins
      , M.fromColumns
        $ map (VS.fromList . snd . histogram1d hmin hmax bins . VS.toList) eachFbin)


mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (VS.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = VS.slice 0 n vIn :  mkChunksCore (VS.drop n vIn) n (m-1)


histogram1d :: Double -> Double -> [Double] -> [Double] -> ([Double], [Double])
histogram1d xmin xmax bins input = do
  let intervals = zipWith (\x y ->(x, y)) (init bins) (tail bins)
      within u x = x >= fst u && x < snd u
   in (map fst intervals, map (fromIntegral.length) $ map (\u -> filter (within u) input) intervals)




