{-******************************************
  *     File Name: Functions.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/02 18:39:10
  *******************************************-}

module HasKAL.FrameUtils.Function (
   readFrame
  ,readFrameV
  ,readFrameFromGPS
  ,readFrameUntilGPS
) where

import qualified Control.Monad as CM
import qualified Data.Packed.Vector as DPV

import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP

readFrame :: String -> String -> IO [Double]
readFrame = (CM.liftM ((map realToFrac).HFF.eval).).HFF.readFrame

readFrameV :: String -> String -> IO (DPV.Vector Double)
readFrameV = (CM.liftM (DPV.fromList.(map realToFrac).HFF.eval).).HFF.readFrame

readFrameFromGPS :: Integer -> Integer -> String -> String -> IO [Double]
readFrameFromGPS gpsTime obsTime channel cache = do
  let fileNames = HFP.pickUpFileNameinFile gpsTime (gpsTime + obsTime - 1) cache
  CM.liftM concat $ mapM (readFrame channel) fileNames -- stack overflowするようならmapM'に置き換える

readFrameUntilGPS :: Integer -> Integer -> String -> String -> IO [Double]
readFrameUntilGPS gpsTime obsTime channel cache = do
  let fileNames = HFP.pickUpFileNameinFile (gpsTime - obsTime) (gpsTime - 1) cache
  CM.liftM concat $ mapM (readFrame channel) fileNames
