

module HasKAL.MonitorUtils.SensMon
( SensParam
, runSensMon
--,
) where

import qualified Data.Packed.Matrix as M
import qualified Data.Vector.Storable as VS
import HasKAL.SpectrumUtils.SpectrumUtils


data SensParam = SensParam
       { histmin :: Double
       , histmax :: Double
       , binInterval :: Double
       }


runSensMon :: VS.Vector Double -> Double -> Int -> Spectrogram
runSensMon input fs n =
  let param = SensParam
        { histmax = 1.0E-18
        , histmin = 1.0E-24
        , binInterval = (logBase 10 (histmax param) - logBase 10 (histmin param))/100.0
        }
   in runSensMonCore input fs n param


runSensMonCore :: VS.Vector Double -> Double -> Int -> SensParam -> Spectrogram
runSensMonCore input fs n param = do
  let chunks = mkChunks input n
      vlist  = map (\x -> VS.take (floor (fromIntegral n/fs))
        $ VS.map (sqrt . (*2)) $ snd $ gwpsdV x n fs) chunks
      eachFbin = M.toColumns . M.fromRows $ vlist
      hmax = histmax param
      hmin = histmin param
      dbins' = binInterval param
      bins' = [logBase 10 hmin, logBase 10 hmin + dbins'..logBase 10 hmax]
      bins = map (10**) bins'
   in ( VS.fromList $ take (length vlist) [0, fromIntegral n/fs..]
      , VS.fromList bins
      , M.fromColumns
        $ map (VS.fromList . snd . histogram1d hmin hmax bins . VS.toList) eachFbin)


mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
--mkChunks [] _ = []
mkChunks vIn n = VS.slice 0 n vIn : mkChunks (VS.drop n vIn) n


histogram1d :: Double -> Double -> [Double] -> [Double] -> ([Double], [Double])
histogram1d xmin xmax bins input = do
  let intervals = zipWith (\x y ->(x, y)) (init bins) (tail bins)
      within u x = x >= fst u && x < snd u
   in (map fst intervals, map (fromIntegral.length) $ map (\u -> filter (within u) input) intervals)




