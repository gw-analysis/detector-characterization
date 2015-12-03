
module HasKAL.SignalProcessingUtils.Kaiser
( genFIRcoeffCore
) 
where


import HasKAL.SignalProcessingUtils.WindowFunction (kaiser)
import HasKAL.SignalProcessingUtils.FilterType (FilterType(..))
import qualified Numeric.LinearAlgebra as NL


genFIRcoeffCore (deltas, fres) (deltap, frep) fs fc = 
  let as = -20 * logBase 10 deltas
      ap = -20 * logBase 10 deltap
      b | as > 50              = 0.1102*(as-8.7)
        | as >= 21 && as <= 50 = 0.5842*(as-21)**0.4+0.07886*(as-21)
        | as < 21              = 0
      n = floor $ 
        (dfunc deltas deltap - (ffunc deltas deltap) * (fres - frep)**2.0) / abs (fres - frep)
        where
          dfunc deltas deltap 
           | deltap>=deltas = 
              (a1*(logBase 10 deltap)**2+a2*(logBase 10 deltap)+a3)*logBase 10 deltas 
              - (a4*(logBase 10 deltap)**2+a5*(logBase 10 deltap)+a6)
           | deltap<deltas  =
              (a1*(logBase 10 deltas)**2+a2*(logBase 10 deltas)+a3)*logBase 10 deltap 
              - (a4*(logBase 10 deltas)**2+a5*(logBase 10 deltas)+a6)
          ffunc deltas deltap
           | deltap>=deltas = b1+b2*(logBase 10 deltap - logBase 10 deltas)
           | deltap<deltas  = b1+b2*(logBase 10 deltas - logBase 10 deltap)
          a1 = 0.005309
          a2 = 0.07114
          a3 = -0.4761
          a4 = 0.00266
          a5 = 0.5941
          a6 = 0.4278
          b1 = 11.01217
          b2 = 0.51244 
      sinc fs fc m = 
        (flip map) [0..m-1] $ \l->2*sin (2*pi*fc*fromIntegral l)/(2*pi*fs* fromIntegral l)
   in zipWith (*) (NL.toList $ kaiser b n) (sinc fs fc n)

