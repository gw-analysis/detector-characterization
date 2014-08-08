module HasKAL.DetectorUtils.Functions
( fplusfcrossts
--, rx
--, ry
--, rz
--, unitVector
--, sind
--, cosd
--, tand
--, datan
--, tuple2mat
--, calcd
) where

import Numeric.LinearAlgebra
--import HasKAL.Constants.Constants
import HasKAL.DetectorUtils.DetectorParam


speedofLight :: Double
speedofLight = 299792458 -- meter/s

{- exposed functions -}
fplusfcrossts :: DetectorParam -> Double -> Double -> Double -> (Double, Double, Double)
fplusfcrossts detname phi theta psi = do
  -- Detector Tensor
  let d = calcd (tuple2mat (deta detname)) (tuple2mat (detb detname))
      rr = rz (90+psi) <> ry (90-theta) <> rz (phi)
      dtensor = rr <> d <> trans rr
      fplus = (dtensor @@> (0, 0) - dtensor @@> (1, 1)) / 2.0
      fcross= -(dtensor @@> (0, 1) + dtensor @@> (1, 0)) / 2.0
      tau = (tuple2mat (detr detname) <> (unitVector phi theta) / (scalar speedofLight)) @@> (0, 0)
  (fplus, fcross, tau)

-- rotation
rx x = (3><3) [1, 0, 0, 0, cosd x, sind x, 0, -sind x, cosd x]
ry x = (3><3) [cosd x, 0, -sind x, 0, 1, 0, sind x, 0, cosd x]
rz x = (3><3) [cosd x, sind x, 0, -sind x, cosd x, 0, 0, 0, 1]

-- unit vector
unitVector :: Double -> Double -> Matrix Double
unitVector phi theta = (3><1) [cosd theta * cosd phi, cosd theta * sind phi, sind theta]

-- trigonometric functions in degree
sind d = sin (d * pi / 180)
cosd d = cos (d * pi / 180)
tand d = tan (d * pi / 180)
datan x = 180 / pi * atan x

tuple2mat :: (Double, Double, Double) -> Matrix Double
tuple2mat (x, y, z) = (1><3) [x, y, z]

calcd :: Matrix Double -> Matrix Double -> Matrix Double
calcd a b = trans a <> a - trans b <> b
