
module HasKAL.WaveUtils.Signature
where


import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V

type GravitationalWave = (Hplus, Hcross)
type Hplus = Vector Double
type Hcross = Vector Double
type TimeSeries = V.Vector Double

lengthTimeSeries :: V.Vector Double
                 -> Int
lengthTimeSeries = V.length
