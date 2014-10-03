{-******************************************
  *     File Name: Functions.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/03 17:20:09
  *******************************************-}

module HasKAL.FrameUtils.Function (
   readFrame
  ,readFrameV
  ,readFrameFromGPS
--  ,readFrameUntilGPS
  ,readFrameWaveData
) where

import qualified Control.Monad as CM
import qualified Data.Packed.Vector as DPV

import qualified HasKAL.DetectorUtils.Detector as HDD
import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.TimeUtils.Function as HTF
import qualified HasKAL.WaveUtils.Data as HWD

readFrame :: String -> String -> IO [Double]
readFrame = (CM.liftM ((map realToFrac).HFF.eval).).HFF.readFrame

readFrameV :: String -> String -> IO (DPV.Vector Double)
readFrameV = (CM.liftM (DPV.fromList.(map realToFrac).HFF.eval).).HFF.readFrame

readFrameFromGPS :: Integer -> Integer -> String -> String -> IO [Double]
readFrameFromGPS gpsTime obsTime channel cache = do
  let fileNames = HFP.pickUpFileNameinFile gpsTime (gpsTime + obsTime - 1) cache
  fs <- HFF.getSamplingFrequency (head fileNames) channel
  fileGPS <- CM.liftM HTF.deformatGPS $ HFF.getGPSTime $ head fileNames
  let headNum = case ((fromIntegral gpsTime) - fileGPS) <= 0 of
        True -> 0
        False -> truncate $ ((fromIntegral gpsTime) - fileGPS) * fs
      length = truncate $ fs * (fromIntegral obsTime)
  CM.liftM ((take length).(drop headNum).concat) $ mapM (readFrame channel) fileNames

-- readFrameUntilGPS :: Integer -> Integer -> String -> String -> IO [Double]
-- readFrameUntilGPS gpsTime obsTime channel cache = do
--   let fileNames = HFP.pickUpFileNameinFile (gpsTime - obsTime) (gpsTime - 1) cache
--   CM.liftM concat $ mapM (readFrame channel) fileNames

readFrameWaveData :: HDD.Detector -> Integer -> Integer -> String -> String -> IO HWD.WaveData
readFrameWaveData detector gpsTime obsTime channel cache = do
  fs <- HFF.getSamplingFrequency (head fileNames) channel
  fileGPS <- CM.liftM HTF.deformatGPS $ HFF.getGPSTime $ head fileNames
  let startGPS = case ((fromIntegral gpsTime) - fileGPS) <= 0 of
        True -> fileGPS
        False -> (truncate $ fs * ((fromIntegral gpsTime) - fileGPS)) / fs + fileGPS
      endGPS = (truncate $ fs * (fromIntegral obsTime)) / fs + startGPS
  soft <- readFrameFromGPS gpsTime obsTime channel cache
  return $ mkWaveData detector "test dayo" fs (deformatGPS startGPS) (deformatGPS endGPS) soft
  