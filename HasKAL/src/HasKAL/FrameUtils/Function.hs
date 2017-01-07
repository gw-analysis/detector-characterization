


module HasKAL.FrameUtils.Function
( readFrameV
, geneTimeVect
, addTimeVect
, readFrameFromGPS
, readFrameFromGPS'V
--  ,readFrameUntilGPS
, readFrameWaveData
, readFrameWaveData'
) where

import qualified Control.Monad as CM
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Maybe (fromJust, fromMaybe)
import qualified Numeric.LinearAlgebra.Data as DPV
import qualified Data.Traversable as DT

import qualified HasKAL.DetectorUtils.Detector as HDD
import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.SpectrumUtils.Signature as HSS
import qualified HasKAL.TimeUtils.Function as HTF
import qualified HasKAL.WaveUtils.Data as HWD

import qualified Data.Vector.Storable as V

dim :: V.Vector Double -> Int
dim = V.length

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
addTimeVect fs dat = (geneTimeVect (dim dat) fs, dat)

readFrameFromGPS :: Integer -- ^ start GPS [sec]
                 -> Integer -- ^ length [sec]
                 -> String -- ^ channel name
                 -> String -- ^ file name
                 -> IO (Maybe [Double])
readFrameFromGPS gpsTime obsTime channel cache = runMaybeT $ MaybeT $ do
  let fileNames = HFP.pickUpFileNameinFile gpsTime (gpsTime + obsTime - 1) cache
  maybefs <- HFF.getSamplingFrequency (head fileNames) channel
  case maybefs of
    Nothing -> return Nothing
    Just fs -> do
      maybegps <- HFF.getGPSTime $ head fileNames
      case maybegps of
        Nothing -> return Nothing
        Just (gpsS, gpsN, _) -> do
          let fileGPS = HTF.deformatGPS (gpsS,gpsN)
              headNum = if (fromIntegral gpsTime - fileGPS) <= 0 then 0
                else truncate $ (fromIntegral gpsTime - fileGPS) * fs
              length = truncate $ fs * fromIntegral obsTime
          DT.sequence $ Just $ CM.liftM (take length.drop headNum.concat) $ CM.forM fileNames (\x -> do
            maybex <- HFF.readFrame channel x
            return $ fromJust maybex)

readFrameFromGPS'V :: Integer -- ^ start GPS [sec]
                   -> Integer -- ^ length [sec]
                   -> String -- ^ channel name
                   -> String -- ^ file name
                   -> IO (Maybe (DPV.Vector Double))
readFrameFromGPS'V gpsTime obsTime channel cache = do
  readFrameFromGPS gpsTime obsTime channel cache >>= \x -> do
    case x of Nothing -> return Nothing
              Just y -> return $ Just $ DPV.fromList y

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
                  -> IO (Maybe HWD.WaveData)
readFrameWaveData detector gpsTime obsTime channel cache = runMaybeT $ MaybeT $ do
  let fileNames = HFP.pickUpFileNameinFile gpsTime (gpsTime + obsTime - 1) cache
  maybefs <- HFF.getSamplingFrequency (head fileNames) channel
  case maybefs of
    Nothing -> return Nothing
    Just fs -> do
      maybegps <- HFF.getGPSTime $ head fileNames
      case maybegps of
        Nothing -> return Nothing
        Just (gpsS,  gpsN,  _) -> do
          let fileGPS = HTF.deformatGPS (gpsS,gpsN)
          let startGPS = if (fromIntegral gpsTime - fileGPS) <= 0
                then fileGPS
                else fromIntegral (truncate $ fs * (fromIntegral gpsTime - fileGPS)) / fs + fileGPS
          let endGPS = fromIntegral (truncate $ fs * fromIntegral obsTime) / fs + startGPS
          readFrameFromGPS gpsTime obsTime channel cache >>= \x ->
            case x of
              Nothing -> return Nothing
              Just y -> return $ Just $ HWD.mkWaveData detector "test dayo" fs (HTF.formatGPS startGPS) (HTF.formatGPS endGPS) $ DPV.fromList y


readFrameWaveData' :: HDD.Detector
                  -> String
                  -> String
                  -> IO (Maybe HWD.WaveData)
readFrameWaveData' detector channel fname = runMaybeT $ MaybeT $ do
  maybefs <- HFF.getSamplingFrequency fname channel
  case maybefs of
    Nothing -> return Nothing
    Just fs -> do
      maybegps <- HFF.getGPSTime fname
      case maybegps of
        Nothing -> return Nothing
        Just (gpsS,  gpsN,  dt) -> do
          HFF.readFrameV channel fname >>= \maybev -> case maybev of
            Nothing -> return Nothing
            Just v -> return $ Just $ HWD.mkWaveData detector channel fs (gpsS, gpsN) (gpsS+floor dt, gpsN+floor (1E9*(dt-fromIntegral (floor dt)))) v



