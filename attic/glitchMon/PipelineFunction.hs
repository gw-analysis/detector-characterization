

module GlitchMon.PipelineFunction
( dataInfo
, frameInfo
, excludeOnePixelIsland
)
where

import Control.Monad ((>>=))
import Control.Monad.State (runState, get, put)
import Data.List (nub, intersect)
import qualified Data.Set as Set
import HasKAL.FrameUtils.FrameUtils

import Numeric.LinearAlgebra
import HasKAL.TimeUtils.Signature(GPSTIME)
import Data.Int (Int32)
import qualified Data.Set as Set

excludeOnePixelIsland :: [(Int, Int)] -> [(Int, Int)]
excludeOnePixelIsland [] = []
excludeOnePixelIsland y@(x:xs) = case (length (intersect y (basePixel25 x)) > 2) of
  True -> intersect y (basePixel5 x) ++ excludeOnePixelIsland xs
  False-> excludeOnePixelIsland xs


addID :: [(Int, Int)] -> ([(Int, Int, Int)],Int)
addID [] = ([],0)
addID z = runState (go z z) 0
  where
    go y (x:xs) = do
      ind <- get
      let a = intersect y (basePixel25 x)
      case (length a > 2) of
        True -> do put (succ ind)
                   let c = [(p,q,ind)|(p,q)<- a]
                   b <- go y xs
                   return (c++b)
        False-> go y xs


regroup :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
regroup n y@((a,b,c):xs)  = do
  let targetID = [y3|(y1,y2,y3) <- y, y1==a,y2==b]
      grouped = concatMap (\x->filter (\(_,_,r)->r==x) y) targetID
      grouped' = [(a,b,n)|(a,b,_)<-grouped]
      newlist =  Set.toList $ Set.difference (Set.fromList grouped) (Set.fromList y)
   in grouped' ++ regroup (succ n) newlist


findOverlap :: [(Int,Int,Int)] -> [[Int]]
findOverlap x = nub $ (flip map) x $ \(a,b,c) -> [y3|(y1,y2,y3)<-x, y1==a, y2==b]


merging :: [[Int]] -> [[Int]]
merging [] = []
merging y@(x:xs) =
  let z = (flip map) [0..length y] $ \i->
            case (Set.null $ Set.intersection (Set.fromList x) (Set.fromList (xs!!i))) of
              False -> (xs!!i, i)
              True  -> ([],0)
      a = nub $ Set.toList . Set.unions . map Set.fromList . fst . unzip $ z
      e' = snd . unzip $ z
      e = [x|x<-e',x/=0]
      newxs = [ xs!!i | i <- [0..length y], not (Set.member i (Set.fromList e))]
   in a : merging newxs


basePixel9 :: (Int, Int) -> [(Int, Int)]
basePixel9 (a, b) = [(a-1, b-1), (a, b-1), (a+1, b-1), (a-1, b), (a, b), (a+1, b), (a-1, b-1), (a, b-1), (a+1, b-1)]


basePixel5 :: (Int, Int) -> [(Int, Int)]
basePixel5 (a, b) = [(a-1, b), (a, b), (a+1, b), (a, b-1), (a, b+1)]


basePixel25 :: (Int, Int) -> [(Int, Int)]
basePixel25 (a, b) = [(a-2, b-2), (a-1, b-2), (a, b-2), (a+1, b-2), (a+2, b-2), (a-2, b-1), (a-1, b-1), (a, b-1), (a+1, b-1), (a+2, b-1), (a-2, b), (a-1, b), (a, b), (a+1, b), (a+2, b), (a-2, b+1), (a-1, b+1), (a, b+1), (a+1, b+1), (a+2, b+1), (a-2, b+2), (a-1, b+2), (a, b+2), (a+1, b+2), (a+2, b+2)]


basePixel25' :: (Int, Int, Int) -> [(Int, Int, Int)]
basePixel25' (a, b, c) = [(a-2, b-2, c), (a-1, b-2, c), (a, b-2, c), (a+1, b-2, c), (a+2, b-2, c), (a-2, b-1, c), (a-1, b-1, c), (a, b-1, c), (a+1, b-1, c), (a+2, b-1, c), (a-2, b, c), (a-1, b, c), (a, b, c), (a+1, b, c), (a+2, b, c), (a-2, b+1, c), (a-1, b+1, c), (a, b+1, c), (a+1, b+1, c), (a+2, b+1, c), (a-2, b+2, c), (a-1, b+2, c), (a, b+2, c), (a+1, b+2, c), (a+2, b+2, c)]


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



