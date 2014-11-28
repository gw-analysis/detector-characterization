{-******************************************
  *     File Name: DetectorNoiseGenerator.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/07/27 15:31:10
  *******************************************-}

module DetectorNoiseGeneratorNoiseMon (
   geneNPSD
  ,geneNPSD'
  ,psd2ts
  ,psd2complex
--   ,geneNonGaussNPSD
) where


import qualified Data.Complex as DC
import Data.Complex (Complex( (:+) ))
import qualified Data.List as DL
import qualified Numeric.LinearAlgebra as NLA

import qualified HasKAL.DetectorUtils.Detector as HDD 
import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.Misc.StrictMapping as HMS
import qualified HasKAL.SpectrumUtils.DetectorSensitivity as HSD
import Numeric.GSL.Fourier

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
    real <- RND.gslRanGaussian rng (sqrt 0.5)
    imag <- RND.gslRanGaussian rng (sqrt 0.5)
    return $ (freq, (curve :+ 0) * (real :+ imag) )

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

psd2complex :: [(Double,Double)] -> IO [DC.Complex Double]
psd2complex sensCurve = do
  rng <- RNG.newRngWithSeed (-1)
  HMS.forM' sensCurve $ \(freq, curve) -> do 
    let scurve = sqrt curve
    real <- RND.gslRanGaussian rng (sqrt 0.5)
    imag <- RND.gslRanGaussian rng (sqrt 0.5)
    let getrayleigh = (scurve :+ 0)*(real :+ imag)
    return $ getrayleigh

psd2ts :: [(Double,Double)]->IO [Double]
psd2ts sensCurve = do
  psdcomplex <- psd2complex sensCurve
  let vpsdcomplex = NLA.fromList psdcomplex
      vtscomplex = ifft vpsdcomplex
  return $ map NLA.realPart $ NLA.toList vtscomplex  
    





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


