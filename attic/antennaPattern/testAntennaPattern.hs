
--module TestAntennaPattern
--where


import HasKAL.DetectorUtils.Functions
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.DetectorUtils.DetectorParam
import Control.Monad (forM)

main :: IO()
main = do
  let psi = 0
      phiV = [-180,-176..180] :: [Double]
      thetaV = [-90, -86..90] :: [Double]
  skymap <- forM phiV $ \phi ->
        forM thetaV $ \theta ->
        return $ sqrt $ (fst3 $ fplusfcrossts kagra phi theta psi)**2
          + (snd3 $ fplusfcrossts kagra phi theta psi)**2

  --print skymap

  skyMapX Linear COLZ " " "Antenna Pattern Skymap of KAGRA" $ genSkymapData phiV thetaV skymap


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

genSkymapData :: [Double] -> [Double] -> [[Double]] -> [(Double,  Double,  Double)]
genSkymapData phiV thetaV skymap = do
  let phiV' = concat [ replicate (length thetaV) x | x <- phiV]
      thetaV'=take (length phiV * length thetaV) $ cycle thetaV
  zip3 phiV' thetaV' (concat skymap)



