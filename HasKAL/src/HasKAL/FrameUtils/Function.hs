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

module HasKAL.FrameUtils.Function
( readFrameV
, geneTimeVect
, addTimeVect
, readFrameFromGPS
, readFrameFromGPS'V
--  ,readFrameUntilGPS
, readFrameWaveData
) where

import qualified Control.Monad as CM
import Control.Monad.Trans.Maybe (runMaybeT,   MaybeT(..))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Packed.Vector as DPV

import qualified HasKAL.DetectorUtils.Detector as HDD
import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.SpectrumUtils.Signature as HSS
import qualified HasKAL.TimeUtils.Function as HTF
import qualified HasKAL.WaveUtils.Data as HWD

-- | read frame data in Vector
readFrameV :: String -- ^ channel name
           -> String -- ^ file name
           -> IO (Maybe (DPV.Vector Double))
readFrameV ch f = runMaybeT $ MaybeT $
  HFF.readFrame ch f >>= \x ->
    case x of Nothing -> return Nothing
              Just y  -> return $ Just $ DPV.fromList y

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
  maybefs <- HFF.getSamplingFrequency (head fileNames) channel
  let fs = fromMaybe (error "no valid file") maybefs
  maybegps <- HFF.getGPSTime $ head fileNames
  let (gpsS, gpsN, _) = fromMaybe (error "no valid file") maybegps
  let fileGPS = HTF.deformatGPS (gpsS,gpsN)
  let headNum = if (fromIntegral gpsTime - fileGPS) <= 0 then 0
        else truncate $ (fromIntegral gpsTime - fileGPS) * fs
      length = truncate $ fs * fromIntegral obsTime
  CM.liftM (take length.drop headNum.concat) $ CM.forM fileNames (\x -> do
    maybex <- HFF.readFrame channel x
    return $ fromJust maybex)

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
  maybefs <- HFF.getSamplingFrequency (head fileNames) channel
  let fs = fromMaybe (error "no valid file") maybefs
  maybegps <- HFF.getGPSTime $ head fileNames
  let (gpsS, gpsN, _) = case maybegps of Nothing -> error "no valid file"
  let fileGPS = HTF.deformatGPS (gpsS,gpsN)
  let startGPS = if (fromIntegral gpsTime - fileGPS) <= 0 then fileGPS
        else fromIntegral (truncate $ fs * (fromIntegral gpsTime - fileGPS)) / fs + fileGPS
      endGPS = fromIntegral (truncate $ fs * fromIntegral obsTime) / fs + startGPS
  soft <- readFrameFromGPS gpsTime obsTime channel cache
  return $ HWD.mkWaveData detector "test dayo" fs (HTF.formatGPS startGPS) (HTF.formatGPS endGPS) $ DPV.fromList soft

