

module GlitchMon.PipelineFunction
( basePixel5
, basePixel9
, basePixel25
, excludeOnePixelIsland
, taggingIsland
) where


import Control.Monad ((>>=))
import Control.Monad.State (runState, get, put)
import Data.Int (Int32)
import Data.List (nub, intersect)
import qualified Data.Set as Set
import HasKAL.FrameUtils.FrameUtils
import HasKAL.TimeUtils.Signature(GPSTIME)
import Numeric.LinearAlgebra

import GlitchMon.Signature


excludeOnePixelIsland :: ((Int, Int) -> [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)]
excludeOnePixelIsland f [] = []
excludeOnePixelIsland f y@(x:xs) = case (length (intersect y (f x)) > 2) of
  True -> intersect y (f x) ++ excludeOnePixelIsland f xs
  False-> excludeOnePixelIsland f xs


taggingIsland :: [(Int,Int)] -> [[(Tile,ID)]]
taggingIsland x = let ids = secondIDing 1 . fst . firstIDing $ x
                      groupIDs = groupingID ids
                   in (flip map) [1..length groupIDs] $ 
                        \i -> [ ((a,b),i)
                              | (a,b,c) <- ids
                              , elem c (groupIDs!!(i-1))
                              ]


firstIDing :: [(Int, Int)] -> ([(Int, Int, Int)],Int)
firstIDing [] = ([],0)
firstIDing z = runState (go z z) 0
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


secondIDing :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
secondIDing n y@((a,b,c):xs)  = do
  let targetID = [y3|(y1,y2,y3) <- y, y1==a,y2==b] 
      grouped = concatMap (\x->filter (\(_,_,r)->r==x) y) targetID
      grouped' = [(a,b,n)|(a,b,_)<-grouped]
      newlist =  Set.toList $ Set.difference (Set.fromList grouped) (Set.fromList y)
   in grouped' ++ secondIDing (succ n) newlist


groupingID :: [(Int,Int,Int)] -> [[Int]]
groupingID = secondStep . firstStep 


firstStep :: [(Int,Int,Int)] -> [[Int]]
firstStep x = nub $ (flip map) x $ \(a,b,c) -> [y3|(y1,y2,y3)<-x, y1==a, y2==b]


secondStep :: [[Int]] -> [[Int]]
secondStep [] = []
secondStep y@(x:xs) = 
  let z = (flip map) [0..length y] $ \i->
            case (Set.null $ Set.intersection (Set.fromList x) (Set.fromList (xs!!i))) of
              False -> (xs!!i, i)
              True  -> ([],0)
      a = nub $ Set.toList . Set.unions . map Set.fromList . fst . unzip $ z
      e' = snd . unzip $ z
      e = [x|x<-e',x/=0]
      newxs = [ xs!!i | i <- [0..length y], not (Set.member i (Set.fromList e))]
   in a : secondStep newxs


basePixel9 :: (Int, Int) -> [(Int, Int)]
basePixel9 (a, b) = [(a-1, b-1), (a, b-1), (a+1, b-1), (a-1, b), (a, b), (a+1, b), (a-1, b-1), (a, b-1), (a+1, b-1)]


basePixel5 :: (Int, Int) -> [(Int, Int)]
basePixel5 (a, b) = [(a-1, b), (a, b), (a+1, b), (a, b-1), (a, b+1)]


basePixel25 :: (Int, Int) -> [(Int, Int)]
basePixel25 (a, b) = [(a-2, b-2), (a-1, b-2), (a, b-2), (a+1, b-2), (a+2, b-2), (a-2, b-1), (a-1, b-1), (a, b-1), (a+1, b-1), (a+2, b-1), (a-2, b), (a-1, b), (a, b), (a+1, b), (a+2, b), (a-2, b+1), (a-1, b+1), (a, b+1), (a+1, b+1), (a+2, b+1), (a-2, b+2), (a-1, b+2), (a, b+2), (a+1, b+2), (a+2, b+2)]


basePixel25' :: (Int, Int, Int) -> [(Int, Int, Int)]
basePixel25' (a, b, c) = [(a-2, b-2, c), (a-1, b-2, c), (a, b-2, c), (a+1, b-2, c), (a+2, b-2, c), (a-2, b-1, c), (a-1, b-1, c), (a, b-1, c), (a+1, b-1, c), (a+2, b-1, c), (a-2, b, c), (a-1, b, c), (a, b, c), (a+1, b, c), (a+2, b, c), (a-2, b+1, c), (a-1, b+1, c), (a, b+1, c), (a+1, b+1, c), (a+2, b+1, c), (a-2, b+2, c), (a-1, b+2, c), (a, b+2, c), (a+1, b+2, c), (a+2, b+2, c)]


