{-******************************************
  *     File Name: test.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/21 16:17:50
  *******************************************-}

import Data.Packed.Vector as G (subVector,fromList,toList,dim)
import qualified Data.Vector.Generic as G2 (map, sum, forM, drop, head)
import Control.Monad (forM)

import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV, getSpectrum, getTimeEvolution, normalizeSpectrogram)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.AppendFunctionHROOT

import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth

main = do
  let channel = "L1:LOSC-STRAIN"
      fsample = 4096 -- [Hz]
      stride = truncate fsample -- 1 [sec]
      aveN = 128 -- average times of Sn(f)
      chunckN = 128
      shiftN = 16
      clusteringN = 16

  data1 <- readFrameV channel "/data/L-L1_LOSC_4_V1-842743808-4096.gwf"
  data2 <- readFrameV channel "/data/L-L1_LOSC_4_V1-842747904-4096.gwf"
  print $ dim data2

  {-- Student-Rayleigh Monitor --}
  -- let snf = gwpsdV (subVector 0 (stride*aveN) data1) stride fsample -- Averaged Spectrum Sn(f)
  --     hfs = gwspectrogramV 0 stride fsample data2 -- Spectrogram h(t, f)
  --     nus = studentRayleighMonV (QUANT 0.99) fsample stride chunckN shiftN clusteringN snf hfs -- nu(t, f)
  -- spectrogramMX LogY COLZ "nu" "SRMon" nus


  {-- Rayleigh Monitor --}
  let snf = gwpsdV (subVector 0 (stride*aveN) data1) stride fsample -- Averaged Spectrum Sn(f)
      hfs = gwspectrogramV 0 stride fsample $ subVector 0 (stride*chunckN*8) data2 -- Spectrogram h(t, f)
      nus = rayleighMonV [0.5, 0.90, 0.95, 0.99] fsample stride clusteringN snf hfs -- nu(t, f)
      colors = [RED,RED,GREEN,GREEN,BLUE,BLUE,PINK,PINK]
  oPlotXV LogXY LinePoint 2 colors ("frequency", "normalized noise level") 0.05 "RMon" ((0,0),(1,5)) $ concatPair nus
    where concatPair [] = []
          concatPair (x:xs) = [fst x, snd x] ++ concatPair xs


  {-- time Series --}
  -- let (coeffH, denomH) = butter 6 4096 100 High
  --     (coeffL, denomL) = butter 6 4096 1000 Low
  --     data2' = fromList $ iirFilter (iirFilter (toList data2) coeffH denomH) coeffL denomL
  -- forM ([0, 16..4096-16]) $ \idx -> do
  --   let name = "./time/time" ++ (show $ truncate idx) ++ ".png"
  --   plotV Linear Line 1 BLUE ("time [sec]", "h(t)") 0/05 "Time Series" name ((0,0),(0,0)) $ 
  --     (fromList [idx,idx+1/fsample..idx+16], subVector (truncate $ idx*fsample) (truncate $ 16*fsample) data2')


  {-- squear amplitude --}
  -- let (coeffH, denomH) = butter 8 4096 100 High
  --     (coeffL, denomL) = butter 8 4096 300 Low
  --     data2' = fromList $ iirFilter (iirFilter (toList data2) coeffH denomH) coeffL denomL
  -- absh <- G2/forM (fromList [0, 4096*1..G/dim data2 - (4096*1)]) $ \idx -> do
  --   return $ flip (/) (4096*1) $ G2/sum $ G2/map (**2) $ subVector idx (4096*1) data2'
  -- plotXV Linear LinePoint 1 BLUE ("time [sec]", "<|h(t)|^2>") 0/05 "square amplitude with 100-300 BPF" ((0,4096),(0,0)) $
  --   (fromList [0, 1..fromIntegral $ G/dim data2 `div` 4096], absh)



  {-- spectrum --}
  -- let (coeffH, denomH) = butter 8 4096 100 High
  --     (coeffL, denomL) = butter 8 4096 300 Low
  --     data2' = fromList $ iirFilter (iirFilter (toList data2) coeffH denomH) coeffL denomL
  -- let hfs = gwspectrogramV 0 stride fsample data2' -- Spectrogram h(t, f)
  -- forM [2000] $ \idx -> do
  --   -- let name = "./spec/spec" ++ (show idx) ++ ".png"
  --   let name = "X11"
  --   plotV LogXY Line 1 BLUE ("frequency [Hz]", "[/Hz]") 0/05 ("spectrum:"++(show idx)++"sec") name ((0,0),(1e-49,1e-34)) $ getSpectrum idx hfs


  {-- spectgram at frequency --}
  -- let snf = gwpsdV (subVector 0 (stride*aveN) data1) stride fsample -- Averaged Spectrum Sn(f)
  --     hfs = gwspectrogramV 0 stride fsample data2 -- Spectrogram h(t, f)
  --     wfs = normalizeSpectrogram snf hfs
  --     hoge = map (flip getTimeEvolution wfs) [128]
  -- let colors = [RED,BLUE,GREEN,YELLOW,PINK,CYAN]
  -- oPlotXV Linear LinePoint 1 colors ("Time [sec]", "[/Hz]") 0.05 "Noise Level at 129-135Hz" ((0,0),(0,0)) hoge

