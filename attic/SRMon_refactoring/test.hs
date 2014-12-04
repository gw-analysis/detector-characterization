{-******************************************
  *     File Name: test.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/04 15:15:56
  *******************************************-}

import qualified Control.Monad as CM

import Data.Matrix.Unboxed as UM
import Data.Vector.Unboxed as UV
import Data.Packed.Matrix as SM
import Data.Packed.Vector as SV

import HasKAL.FrameUtils.Function
import HasKAL.Misc.Flip3param (flip231)
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SpectrumUtils.Function
import HasKAL.SpectrumUtils.SpectrumUtils

import SRMon
import MatrixSupplement

main = do
  let channel = "L1:LOSC-STRAIN"
      fsample = 4096 :: Double
      nT = 4096 -- 1 sec
      aveN = 128
      chunck = 128
      shift = 16
      clusteringF = 16

  data1 <- readFrameV channel "/home/yamamoto/L-L1_LOSC_4_V1-842743808-4096.gwf"
  data2 <- readFrameV channel "/home/yamamoto/L-L1_LOSC_4_V1-842747904-4096.gwf"

  let snf = slice 0 (truncate $ fsample/2) $ convert $ snd $ flip231 gwpsdV nT fsample $ subVector 0 (nT*aveN) data1
      hfs = convertS2U $ trd' $ gwspectrogramV 0 nT fsample data2

      nus = timeShift (srMonM 0.99) chunck shift clusteringF (UV.map sqrt snf) (UM.map ((*(sqrt 2.0)).sqrt) hfs)
     
      nus' = plotFormatedSpectogram (SV.fromList [0,16..3968], SV.fromList [16,32..2048], convertU2S nus)

  spectrogramX LogY COLZ "nu" "SRMon" nus'

trd' :: Spectrogram -> SM.Matrix Double
trd' (tV, fV, specM) = specM
  

