



module HasKAL.SearchUtils.Common.CleanDataFinder
( cleanDataFinderCore
--,
) where

import Numeric.LinearAlgebra (toColumns, fromRows)
import Data.Vector.Algorithms.Heap (select)
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as V
import HasKAL.LineUtils.LineRemoval.RngMedian (rngMedV)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV)
import HasKAL.TimeUtils.Function (deformatGPS, formatGPS)
import HasKAL.TimeUtils.Signature
import HasKAL.WaveUtils.Data (WaveData (..))
import Numeric.GSL.Statistics (stddev)

data CutoffType = Low | High deriving (Show)


cleanDataFinderCore :: Int                        -- ^ block size for noise floor estimation
                    -> Int                        -- ^ nfft
                    -> Int                        -- ^ chunk size
                    -> (Double, Double)          -- ^ (f_L, f_U)
                    -> Double                     -- ^ sample rate
                    -> WaveData 
                    -> [(GPSTIME, Bool)]        -- ^ [(gps,True or Fase)]
cleanDataFinderCore blcksz nfft chunk flu fs wave = do
  let v = gwdata wave
      gps = startGPSTime wave
      chunks = mkChunks v chunk
      psdtrain = flip map chunks $ \x-> gwOnesidedPSDV x nfft fs
      nf = flip map psdtrain $ nfEstimate blcksz flu
      (_, refpsd, refstd) = takeMedian nf
      psds = snd . unzip $ nf
      nlevel = flip map psds $ \x-> V.map abs $ V.zipWith (-) x refpsd
      ratioV = flip map nlevel $ \x-> V.zipWith (/) x (2*refstd)                   -- ^ theadhold
      judge = flip map ratioV $ \x-> do let tlen = V.length $ V.filter (<1.0) x
                                        case (tlen == V.length x) of
                                          True -> True
                                          False-> False
      t0 = deformatGPS gps
      dt = fromIntegral chunk / fs
   in zip (map formatGPS [t0,t0+dt..]) judge


-- | input [(f,psd)]
-- | output (f, menian values of psd)
takeMedian :: [(V.Vector Double, V.Vector Double)]
           -> (V.Vector Double, V.Vector Double, V.Vector Double)
takeMedian vs = let (vf,vlist) = unzip vs
                    fbins = toColumns $ fromRows vlist
                    len = V.length (head fbins)
                    len2= len `div` 2
                 in ( head vf
                    , V.fromList $ flip map fbins $ \v-> vecSelect len2 v
                    , V.fromList $ flip map fbins stddev)


-- | Noise Floor Estimation
nfEstimate :: Int
           -> (Double, Double)
           -> (V.Vector Double, V.Vector Double)
           -> (V.Vector Double, V.Vector Double)
nfEstimate blcksz (fl, fu) psddat = do
  let (fv', dat') = psddat
      maybeflind = findCutoffInd fv' fl Low
      maybefuind = findCutoffInd fv' fu High
  case (maybeflind, maybefuind) of
        (Just flind', Just fuind') -> do let (flind, fuind) = (fst flind', fst fuind')
                                             dat = V.drop flind $ V.take fuind dat'
                                             fv  = V.drop flind $ V.take fuind fv'
                                          in (fv, rngMedV dat (V.length dat) blcksz)
        (Nothing, _) -> error "no f_L"
        (_, Nothing) -> error "no f_U"


-- | find cutoff frequency indexes of frequency vector
findCutoffInd :: V.Vector Double
              -> Double
              -> CutoffType
              -> Maybe (Int, Double)
findCutoffInd input x0 cuttype =
  case cuttype of
    Low -> ascend 0
    High-> descend (V.length input -1)
  where ascend m = if x0 <= input!m
                     then Just (m, input!m)
                     else if m+1 >= V.length input-1
                            then Nothing
                            else ascend (m+1)
        descend m = if x0 >= input!m
                      then Just (m, input!m)
                      else if m-1 <= 0
                             then Nothing
                             else descend (m-1)


-- | quickselect by T. Yamamoto
vecSelect :: Int -> V.Vector Double -> Double
vecSelect k vec = V.head $ V.modify (flip select (k+1)) vec


-- | divide a vector into vectors with n-length
mkChunks :: V.Vector Double -> Int -> [V.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (V.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = V.slice 0 n vIn :  mkChunksCore (V.drop n vIn) n (m-1)



