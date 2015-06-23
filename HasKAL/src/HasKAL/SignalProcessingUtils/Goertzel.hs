


module HasKAL.SignalProcessingUtils.Goertzel
( goertzel
, goertzelCoeff
) where

import HasKAL.SignalProcessingUtils.Filter
import qualified Data.Vector.Storable as VS
import Data.Complex

goertzel :: VS.Vector Double -> Double -> Double -> (Double, Complex Double)
goertzel vec fs f = do
  let y' = iir (goertzelCoeff fs nvec f) vec
      yn = VS.last y'
      ym = y' VS.! (nvec-2)
      scale = nvec'/2.0
      y = (yn - cos (2*pi*k/nvec') * ym)/scale :+ (- sin (2*pi*k/nvec') * ym)/scale
  (f, y)
  where
    nvec = VS.length vec
    nvec' = fromIntegral nvec :: Double
    k = f/fs*fromIntegral nvec :: Double


goertzelCoeff :: Double -> Int -> Double -> ([Double], [Double])
goertzelCoeff fs n f = ([1,0, 0], [1, -2*cos (2*pi*k/n'), 1])
  where k = f/fs*n' :: Double
        n' = fromIntegral n :: Double




