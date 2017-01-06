{-# LANGUAGE FlexibleContexts #-}

module HasKAL.MonitorUtils.GlitchMon.PipelineFunction
( basePixel5
, basePixel9
, basePixel25
, excludeOnePixelIsland
, taggingIsland
, selectSegment
-- for debug
, firstIDing
, secondIDing
, groupingID
, firstStep
, secondStep
) 
where


import Control.Monad ((>>=))
import Control.Monad.State (execState, evalState, runState, get, put)
import Data.Int (Int32)
import Data.List (nub, intersect)
import qualified Data.Set as Set
import HasKAL.FrameUtils.FrameUtils
import HasKAL.TimeUtils.Signature(GPSTIME)
import Numeric.LinearAlgebra
import System.IO.Unsafe (unsafePerformIO)
import HasKAL.MonitorUtils.GlitchMon.Signature


dataSegmentation :: FilePath
                 -> Int
                 -> [(Int,Int)]
dataSegmentation f sec = unsafePerformIO $ do
  gpslist' <- return $ readFile f >>= \x -> return $ lines x
  let gpslist = unsafePerformIO $ fmap (map (toTuple.words)) gpslist'
  return $ concat $ (flip map) gpslist $ \(gps,dt) -> 
    let n = dt `div` sec
     in [(gps+i*sec,sec)|i<-[0..n-1]]


selectSegment :: Int -> FilePath -> [(Int,Int)]
selectSegment dur_sec fpath = unsafePerformIO $ do
  gpslist <- return $ readFile fpath >>= \x -> return $ lines x
  tmplist <- fmap (map (toTuple.words)) gpslist >>= \l -> return $ filter (\(gps,d) -> d >= dur_sec) l
  return $ concat $ (flip map) tmplist $ \(gps,dt) -> 
    let n = dt `div` dur_sec
     in [(gps+i*dur_sec,dur_sec)|i<-[0..n-1]]
 

--excludeOnePixelIsland :: ((Int, Int) -> [(Int, Int)]) -> Int -> [(Int, Int)] -> [(Int, Int)]
--excludeOnePixelIsland f n [] = []
--excludeOnePixelIsland f n y'@(x':xs') = do
--  let x = Set.singleton x'
--      y = Set.fromList y'
--      xs= Set.fromList xs' 
--      fx= Set.fromList (f x')
--      iy= Set.intersection y fx
--   in case (Set.size iy >= n) of
--        True -> Set.toList iy ++ excludeOnePixelIsland f n xs'
--        False-> excludeOnePixelIsland f n xs'


excludeOnePixelIsland :: ((Int, Int) -> [(Int, Int)]) -> Int -> [(Int, Int)] -> [(Int, Int)]
excludeOnePixelIsland f n [] = []
excludeOnePixelIsland f n y@(x:xs) = case (length (intersect y (f x)) >= n) of
  True -> intersect y (f x) ++ excludeOnePixelIsland f n xs
  False-> excludeOnePixelIsland f n xs



taggingIsland :: ((Int, Int) -> [(Int, Int)]) -> Int -> [(Int,Int)] -> [[(Tile,ID)]]
taggingIsland cfun minN x = let ids = mynub . secondIDing 1 . fst . firstIDing cfun minN $ x
                                --ids = mynub . fst . firstIDing cfun minN $ x
                                groupIDs = groupingID ids
                   in map mynub $ (flip map) [1..length groupIDs] $ 
                        \i -> [ ((a,b),i)
                              | (a,b,c) <- ids
                              , elem c (groupIDs!!(i-1))
                                  ]


firstIDing :: ((Int, Int) -> [(Int, Int)]) -> Int -> [(Int, Int)] -> ([(Int, Int, Int)],Int)
firstIDing cfun minN [] = ([],0)
firstIDing cfun minN z = runState (go z z) 0
  where
    go y (x:xs) = do 
      ind <- get
      let a = intersect y (cfun x)
      case (length a > minN) of
        True -> do put (succ ind)
                   let c = [(p,q,ind)|(p,q)<- a]
                   b <- go y xs
                   return (c++b)
        False-> go y xs
    go y [] = return []


secondIDing :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
secondIDing n y@((a,b,c):xs)  = do
  let targetID = [y3|(y1,y2,y3) <- y, y1==a,y2==b] 
      grouped = concatMap (\x->filter (\(_,_,r)->r==x) y) targetID
      grouped' = [(a,b,n)|(a,b,_)<-grouped]
--      newlist =  Set.toList $ Set.difference (Set.fromList grouped) (Set.fromList y)
      newlist = filter (\(x1,x2,x3)->not $ x3 `elem` targetID) y
   in secondIDing (succ n) newlist ++ grouped'
secondIDing n y@[] = []


groupingID :: [(Int,Int,Int)] -> [[Int]]
groupingID = secondStep . firstStep 


firstStep :: [(Int,Int,Int)] -> [[Int]]
firstStep x = mynub $ (flip map) x $ \(a,b,c) -> [y3|(y1,y2,y3)<-x, y1==a, y2==b]


secondStep :: [[Int]] -> [[Int]]
secondStep [] = []
secondStep a = evalState (go a) (length . mynub . concat $ a)
 where
  go [] = return []
  go x  = do
     i <- get
     case (i > 0) of
       True -> do
         let a = mynub . concat $ [ids|ids<-x, Set.member i (Set.fromList ids)] 
             b = [ids|ids<-x, not (Set.member i (Set.fromList ids))]
         put (i-1)
         go (a : b)
       False -> do 
         return x


-- have bug
secondStep' :: [[Int]] -> [[Int]]
secondStep' [] = []
secondStep' a = execState (go a) []
  where
    go [] = return []
    go (x:xs) = do
      tbl <- get
      let z = (flip map) [0..length xs-1] $ \i->
            case (Set.null $ Set.intersection (Set.fromList x) (Set.fromList (xs!!i))) of
              False -> (xs!!i,i)
              True  -> ([],-1)
          a = mynub $ Set.toList . Set.unions . map Set.fromList . fst . unzip $ z
          e' = snd . unzip $ z
          e = [x|x<-e',x/=(-1)]
          newxs = [ xs!!i | i <- [0..length xs-1], not (Set.member i (Set.fromList e))]
       in case (length a) of 
            0 -> do put (x:tbl)
                    go xs
            _ -> case newxs of 
                   [] -> do let b = mynub . Set.toList . Set.unions 
                                  $ [Set.fromList a,Set.fromList x]
                            put (b:tbl)   
                            return []
                   _  -> do put (a:tbl)
                            go (a:newxs)


basePixel9 :: (Int, Int) -> [(Int, Int)]
basePixel9 (a, b) = [(a-1, b-1), (a, b-1), (a+1, b-1), (a-1, b), (a, b), (a+1, b), (a-1, b+1), (a, b+1), (a+1, b+1)]


basePixel5 :: (Int, Int) -> [(Int, Int)]
basePixel5 (a, b) = [(a-1, b), (a, b), (a+1, b), (a, b-1), (a, b+1)]


basePixel25 :: (Int, Int) -> [(Int, Int)]
basePixel25 (a, b) = [(a-2, b-2), (a-1, b-2), (a, b-2), (a+1, b-2), (a+2, b-2), (a-2, b-1), (a-1, b-1), (a, b-1), (a+1, b-1), (a+2, b-1), (a-2, b), (a-1, b), (a, b), (a+1, b), (a+2, b), (a-2, b+1), (a-1, b+1), (a, b+1), (a+1, b+1), (a+2, b+1), (a-2, b+2), (a-1, b+2), (a, b+2), (a+1, b+2), (a+2, b+2)]


basePixel25' :: (Int, Int, Int) -> [(Int, Int, Int)]
basePixel25' (a, b, c) = [(a-2, b-2, c), (a-1, b-2, c), (a, b-2, c), (a+1, b-2, c), (a+2, b-2, c), (a-2, b-1, c), (a-1, b-1, c), (a, b-1, c), (a+1, b-1, c), (a+2, b-1, c), (a-2, b, c), (a-1, b, c), (a, b, c), (a+1, b, c), (a+2, b, c), (a-2, b+1, c), (a-1, b+1, c), (a, b+1, c), (a+1, b+1, c), (a+2, b+1, c), (a-2, b+2, c), (a-1, b+2, c), (a, b+2, c), (a+1, b+2, c), (a+2, b+2, c)]


-- | O (nlog n) nub 
-- | lent from http://d.hatena.ne.jp/jeneshicc/20090908/1252413541
mynub :: (Ord a) => [a] -> [a]
mynub l = nub'' l Set.empty
   where nub'' [] _ = []
         nub'' (x:xs) s
           | x `Set.member` s = nub'' xs s
           | otherwise    = x : nub'' xs (x `Set.insert` s)


toTuple :: [String] 
        -> (Int,Int)
toTuple x = (read (head x) :: Int,read (x!!1) :: Int)



