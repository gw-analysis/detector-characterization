
module HasKAL.SignalProcessingUtils.Kaiser
( genFIRcoeff
, FIRparam(..)
, mkFIRparam
, update'ripplePassband
, update'freqPassband
, update'rippleStopband
, update'freqStopband
, update'sampleRate
, update'freqCutoff1
, update'freqCutoff2
, genFIRcoeffLowCore
, genFIRcoeffHighCore
, genFIRcoeffBandPassCore
) 
where


import HasKAL.SignalProcessingUtils.WindowFunction (kaiser)
import HasKAL.SignalProcessingUtils.FilterType (FilterType(..))
import qualified Numeric.LinearAlgebra as NL


data FIRparam = FIRparam { ripplePassband :: Double
                         , freqPassband   :: Double
                         , rippleStopband :: Double
                         , freqStopband   :: Double
                         , sampleRate     :: Double
                         , freqCutoff1    :: Double
                         , freqCutoff2    :: Double
                         } deriving (Show)


mkFIRparam a1 a2 a3 a4 a5 a6 a7 = 
  FIRparam { ripplePassband = a1 
           , freqPassband   = a2
           , rippleStopband = a3
           , freqStopband   = a4
           , sampleRate     = a5
           , freqCutoff1    = a6
           , freqCutoff2    = a7
           }


update'ripplePassband p x = p {ripplePassband = x}
update'freqPassband p x = p {freqPassband = x}
update'rippleStopband p x = p {rippleStopband = x}
update'freqStopband p x = p {freqStopband = x}
update'sampleRate p x = p {sampleRate = x}
update'freqCutoff1 p x = p {freqCutoff1 = x}
update'freqCutoff2 p x = p {freqCutoff2 = x}


genFIRcoeff param filttype
  | filttype == Low      = genFIRcoeffLowCore param
  | filttype == High     = genFIRcoeffHighCore param
  | filttype == BandPass = genFIRcoeffBandPassCore param


genFIRcoeffLowCore param = 
  let deltas = rippleStopband param
      fres   = freqStopband param
      deltap = ripplePassband param
      frep   = freqPassband param
      fs     = sampleRate param
      fc     = freqCutoff1 param

      as = -20 * logBase 10 deltas
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


genFIRcoeffHighCore param = 
  let fs  = sampleRate param
      fc  = freqCutoff1 param
      fc' = fs/2 - fc
      param' = update'freqCutoff1 param fc'
   in zipWith (*) (cycle [1,-1]) $ genFIRcoeffLowCore param'


genFIRcoeffBandPassCore param =
  let fs     = sampleRate param
      fc1    = freqCutoff1 param
      fc2    = freqCutoff2 param
      omega0 = 2*pi*(fc1+fc2)/2
      fc'    = (fc2-fc1)/2
      param' = update'freqCutoff1 param fc'
   in zipWith (*) [2*2*cos (omega0/fs*fromIntegral m)|m<-[0..]]
         $ genFIRcoeffLowCore param'


 

