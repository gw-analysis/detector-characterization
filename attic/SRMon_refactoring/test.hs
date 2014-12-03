{-******************************************
  *     File Name: test.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/03 21:44:56
  *******************************************-}

import qualified HasKAL.FrameUtils.FrameUtils as HFF -- データ読み出し
import qualified Control.Monad as CM

import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS
import qualified HasKAL.SpectrumUtils.Function as HSF
import qualified HasKAL.Misc.Flip3param as HMF 

import Data.Matrix.Unboxed as UM
import Data.Vector.Unboxed as UV
import Data.Packed.Vector as SV
import Data.Packed.Matrix as SM

import SRMon

import HasKAL.PlotUtils.HROOT.PlotGraph3D as PG3

main = do
  let channel = "L1:LOSC-STRAIN"
      fsample = 4096 :: Double
      nT = 4096 -- 1 sec
      aveN = 128
      chunck = 128
      shift = 16
      clusteringF = 16

  data1 <- readFrame' channel "/home/yamamoto/L-L1_LOSC_4_V1-842743808-4096.gwf"
  data2 <- readFrame' channel "/home/yamamoto/L-L1_LOSC_4_V1-842747904-4096.gwf"

  let snf = slice 0 (truncate $ fsample/2) $ convert $ snd $ HMF.flip231 HSS.gwpsdV nT fsample $ convert $ slice 0 (nT*aveN) $ UV.fromList data1
  -- let snf = convert $ snd $ HMF.flip231 HSS.gwpsdV nT fsample $ convert $ slice 0 (nT*aveN) $ UV.fromList data1
      hfs = tr $ convertS2U $ trd' $ HSS.gwspectrogramV 0 nT fsample $ convert $ UV.fromList data2

      nus = UM.fromRows $ timeShift (srMonM 0.99) chunck shift clusteringF (UV.map sqrt snf) (UM.map ((*(sqrt 2.0)).sqrt) hfs)
     
      nus' = timeFreqData [0,16..] [16,32..2048] $ UM.toLists nus

  -- print "### length of Sn(f) ###"
  -- print $ UV.length snf
  -- print "### dim of h(f,t)  (cols, rows) ###"
  -- print (UM.cols hfs, UM.rows hfs)
  -- print "### dim of nu(f,t)  (cols, rows) ###"
  -- print (UM.cols nus, UM.rows nus) -- 数が合わない
  -- print $ Prelude.map UV.toList nus
  spectrogramX LogY COLZ "nu" "SRMon" nus'

trd' :: HSS.Spectrogram -> SM.Matrix Double
trd' (tV, fV, specM) = specM
  

readFrame' :: String -> String -> IO [Double]
readFrame' = (CM.liftM ((Prelude.map realToFrac).HFF.eval).).HFF.readFrame

timeFreqData :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
timeFreqData [] _ _ = []
timeFreqData _ [] _ = []
timeFreqData _ _ [] = []
timeFreqData (x:xs) ys (zs:zss) = Prelude.zip3 [x,x..] ys zs Prelude.++ timeFreqData xs ys zss


convertS2U :: SM.Matrix Double -> UM.Matrix Double
convertS2U mat = fromVector rowN colN$ convert $ SM.flatten mat
  where rowN = SM.rows mat
        colN = SM.cols mat

convertU2S :: UM.Matrix Double -> SM.Matrix Double
convertU2S mat = SM.reshape colN $ convert $ UM.flatten mat
  where colN = UM.cols mat


