

module SensMon
( runSensMon
--,
) where

import qualified Data.Packed.Matrix as M
import qualified Data.Vector.Storable as VS
import HasKAL.SpectrumUtils.SpectrumUtils
-- mport HasKAL.SpectrumUtils.Signature


runSensMonCore :: VS.Vector Double -> Double -> Int -> Spectrogram
runSensMonCore input fs n = do
  let chunks = mkChunks input n
      vlist  = map (\x -> snd $ gwpsd x n fs)
      eachFbin = M.toColumns . M.fromRows $ vlist



mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunks [] _ = []
mkChunks vIn n = VS.slice vIn 0 n : mkChunks (VS.drop n vIn) 0 n


histogram1d :: Double -> Double -> Double -> [Double] -> ([Double], [Double])
histogram1d xmin xmax dbin input = do
  let bins = [xmin, (xmin+dbin)..(xmax+dbin)]
      intervals = zipWith (\x y ->(x, y)) (init bins) (tail bins)
      within u x = x >= fst u && x < snd u
   in (map fst intervals, map length $ map (\u -> filter (within u) input) intervals)

