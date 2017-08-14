

module HasKAL.SimulationUtils.DetectorNoiseGenerator
( geneNPSD
, geneNPSDV
, geneNPSD'
, genDetNoise
--   ,geneNonGaussNPSD
) where


import qualified Data.Complex as DC
import Data.Complex (Complex( (:+) ))
import qualified Data.List as DL
import qualified Numeric.LinearAlgebra as NLA
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as V
import System.IO.Unsafe (unsafePerformIO)

import qualified HasKAL.DetectorUtils.Detector as HDD
import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.Misc.StrictMapping as HMS
import qualified HasKAL.SignalProcessingUtils.FilterDesign as FD
import qualified HasKAL.SignalProcessingUtils.FilterX as FX
import qualified HasKAL.SpectrumUtils.DetectorSensitivity as HSD
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)


{-- Test Code --}
-- import qualified HasKAL.PlotUtils.PlotUtilsHROOT as HPP
-- import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as HPO
-- main :: IO ()
-- main = do
--   rng <- RNG.newRngWithSeed (-1)
--   noise1 <- geneNPSD rng HDD.KAGRA [10.0..4000]
--   HPP.plotSaveAsPicture (map fst noise1) (map (DC.magnitude.snd) noise1) "" "" HPO.LogXY HPO.Line "a.png"

--   let num = 2
--   noise2 <- geneNPSD' num HDD.KAGRA [10.0..4000]
--   HPP.plotSaveAsPicture (map fst (noise2 !! 0)) (map (DC.magnitude.snd) (noise2 !! 0)) "" "" HPO.LogXY HPO.Line "b.png"
--   HPP.plotSaveAsPicture (map fst (noise2 !! 1)) (map (DC.magnitude.snd) (noise2 !! 1)) "" "" HPO.LogXY HPO.Line "c.png"


{-- External Functions --}
-- param1: Random Number Generator in GSL
-- param2: Detecotr Type (defined at HasKAL/DetectorUtils/Detector.hs)
-- param3: frequency [Hz] (like as [1.0..1000.0])
-- return: 1st member -> frequency [Hz]
--         2nd member -> h-of-f [/rHz] (real :+ imaginary)
geneNPSD :: RNG.GSLRng -> HDD.Detector -> [Double] -> IO [(Double, DC.Complex Double)]
geneNPSD rng ifo fin = do
  let sensCurve = (zip fin).(map sqrt).NLA.toList.(HSD.ifonoisepsd ifo).NLA.fromList $ fin

  HMS.forM' sensCurve $ \(freq, curve) -> do
    case freq == 0 of
     True -> return $ (freq, 0.0:+0.0)
     False -> do
       real <- RND.gslRanGaussian rng (sqrt 0.5)
       imag <- RND.gslRanGaussian rng (sqrt 0.5)
       return $ (freq, (curve :+ 0) * (real :+ imag) )

geneNPSDV :: RNG.GSLRng -> HDD.Detector -> V.Vector Double -> IO (V.Vector Double, V.Vector (DC.Complex Double))
geneNPSDV rng ifo fin = do
  let sensCurve = (V.map sqrt).(HSD.ifonoisepsd ifo) $ fin

  psd <- V.forM (V.fromList [0..V.length fin - 1]) $ \i -> do
    case fin!i == 0 of
     True -> return $ 0.0:+0.0
     False -> do
       real <- RND.gslRanGaussian rng (sqrt 0.5)
       imag <- RND.gslRanGaussian rng (sqrt 0.5)
       return $ ((sensCurve!i) :+ 0) * (real :+ imag)
  return (fin, psd)

-- param1: Number of generating spectrum
-- param2: Detecotr Type (defined at HasKAL/DetectorUtils/Detector.hs)
-- param3: frequency [Hz] (like as [1.0..1000.0])
-- return: 1st member -> frequency [Hz]
--         2nd member -> h-of-f [/rHz] (real :+ imaginary)
geneNPSD' :: Int -> HDD.Detector -> [Double] -> IO [[(Double, DC.Complex Double)]]
geneNPSD' num ifo fin = do
  let sensCurve = (zip fin).(map sqrt).NLA.toList.(HSD.ifonoisepsd ifo).NLA.fromList $ fin
  rng <- RNG.newRngWithSeed (-1)

  HMS.forM' [1..num] $ \idx -> do
    HMS.forM' sensCurve $ \(freq, curve) -> do
      real <- RND.gslRanGaussian rng (sqrt 0.5)
      imag <- RND.gslRanGaussian rng (sqrt 0.5)
      return $ (freq, (curve :+ 0) * (real :+ imag) )


{--  Unconfirmed  --}
-- param1: Random Number Generator in GSL
-- param2: Detecotr Type (defined at HasKAL/DetectorUtils/Detector.hs)
-- param3: frequency [Hz] (like as [1.0..1000.0])
-- return: 1st member -> frequency [Hz]
--         2nd member -> h-of-f [/rHz] (real :+ imaginary)
-- geneNonGaussNPSD :: RNG.GSLRng -> HDD.Detector -> [Double] -> Double -> IO [(Double, DC.Complex Double)]
-- geneNonGaussNPSD rng ifo fin nu = do
--   let sensCurve = (zip fin).(map sqrt).NLA.toList.(HSD.ifonoisepsd ifo).NLA.fromList $ fin

--   HMS.forM' sensCurve $ \(freq, curve) -> do
--     real <- RND.gslRanGaussian rng nu
--     imag <- RND.gslRanGaussian rng nu
--     return $ (freq, (curve/nu :+ 0) * (real :+ imag) )


genDetNoise :: HDD.Detector
            -> Int
            -> Double
            -> Double
            -> V.Vector Double
genDetNoise ifo ntap fs dt = unsafePerformIO $ do
  gps <- getCurrentGps >>= \t -> return (read t :: Int)
  let n = floor (fs*(dt+2)) :: Int
      randDat = NLA.randomVector gps NLA.Gaussian n
      f = [0..fs/2] :: [Double]
      vf = V.fromList f :: V.Vector Double
      psd = HSD.ifonoisepsd ifo vf :: V.Vector Double
      spe = V.map (sqrt . (*fs)) psd
      (b, a) = FD.fir2 ntap (vf, spe)
      filtered = FX.filtfilt0 (b,a) randDat
  return (V.drop (floor fs) . V.take (n-floor fs) $ filtered)
