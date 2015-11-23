

module GlitchMon.PipelineFunction
( dataInfo
, frameInfo
, excludeOnePixelIsland
)
where

import Control.Monad ((>>=))
import Data.List (intersect)
import qualified Data.Set as Set
import HasKAL.FrameUtils.FrameUtils

import Numeric.LinearAlgebra
import HasKAL.TimeUtils.Signature(GPSTIME)
import Data.Int (Int32)


excludeOnePixelIsland :: [(Int, Int)] -> [(Int, Int)]
excludeOnePixelIsland [] = []
excludeOnePixelIsland y@(x:xs) = case (length (intersect y (basePixel25 x)) > 2) of
  True -> intersect y (basePixel5 x) ++ excludeOnePixelIsland xs
  False-> excludeOnePixelIsland xs


basePixel9 :: (Int, Int) -> [(Int, Int)]
basePixel9 (a, b) = [(a-1, b-1), (a, b-1), (a+1, b-1), (a-1, b), (a, b), (a+1, b), (a-1, b-1), (a, b-1), (a+1, b-1)]


basePixel5 :: (Int, Int) -> [(Int, Int)]
basePixel5 (a, b) = [(a-1, b), (a, b), (a+1, b), (a, b-1), (a, b+1)]


basePixel25 :: (Int, Int) -> [(Int, Int)]
basePixel25 (a, b) = [(a-2, b-2), (a-1, b-2), (a, b-2), (a+1, b-2), (a+2, b-2), (a-2, b-1), (a-1, b-1), (a, b-1), (a+1, b-1), (a+2, b-1), (a-2, b), (a-1, b), (a, b), (a+1, b), (a+2, b), (a-2, b+1), (a-1, b+1), (a, b+1), (a+1, b+1), (a+2, b+1), (a-2, b+2), (a-1, b+2), (a, b+2), (a+1, b+2), (a+2, b+2)]


dataInfo :: String -> String -> IO (Maybe (Vector Double, Double, GPSTIME, Double))
dataInfo cachefile chname = do
  flist <- readFile cachefile
  let fname = head.lines $ flist
  getChannelList fname >>= \x -> case x of
    Nothing -> do
      print "no valid channel in the input frame file"
      return Nothing
    Just y -> do
      let chname' = (fst . head)  y
      case (chname'==chname) of
        False -> return Nothing
        True  -> do
          readFrameV chname fname >>= \x -> case x of
            Nothing -> do
              print "cannot read the file"
              return Nothing
            Just fdata -> do
              getSamplingFrequency fname chname >>= \x -> case x of
                Nothing -> do
                  print "cannot read sampling frequency"
                  return Nothing
                Just fs -> do
                  getGPSTime fname >>= \x -> case x of
                    Nothing -> do
                      print "cannot read GPS time"
                      return Nothing
                    Just (gpsS, gpsN, dt) -> do
                      return $ Just $ (fdata, fs, (gpsS, gpsN), dt)


frameInfo :: String -> String -> IO (Maybe (Vector Double, Double, GPSTIME, Double))
frameInfo fname chname = do
  getChannelList fname >>= \x -> case x of
    Nothing -> do
      print "no valid channel in the input frame file"
      return Nothing
    Just y -> do
      let chname' = (fst . head)  y
      case (chname'==chname) of
        False -> return Nothing
        True  -> do
          readFrameV chname fname >>= \x -> case x of
            Nothing -> do
              print "cannot read the file"
              return Nothing
            Just fdata -> do
              getSamplingFrequency fname chname >>= \x -> case x of
                Nothing -> do
                  print "cannot read sampling frequency"
                  return Nothing
                Just fs -> do
                  getGPSTime fname >>= \x -> case x of
                    Nothing -> do
                      print "cannot read GPS time"
                      return Nothing
                    Just (gpsS, gpsN, dt) -> do
                      return $ Just $ (fdata, fs, (gpsS, gpsN), dt)



