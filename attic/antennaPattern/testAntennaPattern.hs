
--module TestAntennaPattern
--where


import HasKAL.DetectorUtils.Function
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
        return $ sqrt $ (fst $ fst $ fplusfcrossts kagra phi theta psi)**2
          + (snd $ fst $ fplusfcrossts kagra phi theta psi)**2

  --print skymap

  skyMapX Linear COLZ " " "Antenna Pattern Skymap of KAGRA" $ genSkymapData phiV thetaV skymap



genSkymapData :: [Double] -> [Double] -> [[Double]] -> [(Double,  Double,  Double)]
genSkymapData phiV thetaV skymap = do
  let phiV' = concat [ replicate (length thetaV) x | x <- phiV]
      thetaV'=take (length phiV * length thetaV) $ cycle thetaV
  zip3 phiV' thetaV' (concat skymap)



