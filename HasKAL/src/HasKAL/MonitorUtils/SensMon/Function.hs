


module HasKAL.MonitorUtils.SensMon.Function
( updateSensMonInternal
--,
) where


import HasKAL.MonitorUtils.SensMon.Signature
import Numeric.LinearAlgebra


updateSensMonInternal :: SensSpectrum -> Matrix Double -> SensSpectrum
updateSensMonInternal s m = (f, h, m)
  where (f, _, _) = s
        (_, h, _) = s
