{- |
Module      : HasKAL.FrameUtils.Functions
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

Frame IO functions
-}

module HasKAL.FrameUtils.Function (
   readFrame
  ,readFrameV
  ,geneTimeVect
  ,addTimeVect
  ,readFrameFromGPS
  ,readFrameFromGPS'V
--  ,readFrameUntilGPS
  ,readFrameWaveData
) where

import qualified Control.Monad as CM
import qualified Data.Packed.Vector as DPV

import qualified HasKAL.DetectorUtils.Detector as HDD
import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.SpectrumUtils.Signature as HSS
import qualified HasKAL.TimeUtils.Function as HTF
import qualified HasKAL.WaveUtils.Data as HWD

-- | read frame data in List
readFrame :: String -- ^ channel name
          -> String -- ^ file name
          -> IO [Double]
readFrame = (CM.liftM ((map realToFrac).HFF.eval).).HFF.readFrame

-- | read frame data in Vector
readFrameV :: String -- ^ channel name
           -> String -- ^ file name
           -> IO (DPV.Vector Double)
readFrameV = (CM.liftM (DPV.fromList.(map realToFrac).HFF.eval).).HFF.readFrame


geneTimeVect :: Int -> Double -> DPV.Vector Double
geneTimeVect ndata fs = DPV.fromList [0, 1/fs..(fromIntegral ndata - 1)/fs]

addTimeVect :: Double -> DPV.Vector Double -> HSS.Spectrum
addTimeVect fs dat = (geneTimeVect (DPV.dim dat) fs, dat)

readFrameFromGPS :: Integer -- ^ start GPS [sec]
                 -> Integer -- ^ length [sec]
                 -> String -- ^ channel name
                 -> String -- ^ file name
                 -> IO [Double]
readFrameFromGPS gpsTime obsTime channel cache = do
  let fileNames = HFP.pickUpFileNameinFile gpsTime (gpsTime + obsTime - 1) cache
  fs <- HFF.getSamplingFrequency (head fileNames) channel
  fileGPS <- CM.liftM HTF.deformatGPS $ HFF.getGPSTime $ head fileNames
  let headNum = case ((fromIntegral gpsTime) - fileGPS) <= 0 of
        True -> 0
        False -> truncate $ ((fromIntegral gpsTime) - fileGPS) * fs
      length = truncate $ fs * (fromIntegral obsTime)
  CM.liftM ((take length).(drop headNum).concat) $ mapM (readFrame channel) fileNames

readFrameFromGPS'V :: Integer -- ^ start GPS [sec]
                   -> Integer -- ^ length [sec]
                   -> String -- ^ channel name
                   -> String -- ^ file name
                   -> IO (DPV.Vector Double)
readFrameFromGPS'V gpsTime obsTime channel cache = 
  CM.liftM DPV.fromList $ readFrameFromGPS gpsTime obsTime channel cache

-- readFrameUntilGPS :: Integer -> Integer -> String -> String -> IO [Double]
-- readFrameUntilGPS gpsTime obsTime channel cache = do
--   let fileNames = HFP.pickUpFileNameinFile (gpsTime - obsTime) (gpsTime - 1) cache
--   CM.liftM concat $ mapM (readFrame channel) fileNames

-- | read frame data in WaveData format
readFrameWaveData :: HDD.Detector 
                  -> Integer -- ^ start GPS [sec]
                  -> Integer -- ^ length [sec]
                  -> String -- ^ channel name
                  -> String -- ^ file name
                  -> IO HWD.WaveData
readFrameWaveData detector gpsTime obsTime channel cache = do
  let fileNames = HFP.pickUpFileNameinFile gpsTime (gpsTime + obsTime - 1) cache
  fs <- HFF.getSamplingFrequency (head fileNames) channel
  fileGPS <- CM.liftM HTF.deformatGPS $ HFF.getGPSTime $ head fileNames
  let startGPS = case ((fromIntegral gpsTime) - fileGPS) <= 0 of
        True -> fileGPS
        False -> fromIntegral (truncate $ fs * ((fromIntegral gpsTime) - fileGPS)) / fs + fileGPS
      endGPS = fromIntegral (truncate $ fs * (fromIntegral obsTime)) / fs + startGPS
  soft <- readFrameFromGPS gpsTime obsTime channel cache
  return $ HWD.mkWaveData detector "test dayo" fs (HTF.formatGPS startGPS) (HTF.formatGPS endGPS) $ DPV.fromList soft
  
