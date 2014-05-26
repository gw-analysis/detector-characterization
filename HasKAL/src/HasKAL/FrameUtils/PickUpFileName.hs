{-****************************************************
 *PickUpFileName.hs
 *Created:2014/05/20
 *Author:Mitsuhiro Asano
 ****************************************************
Last Modified: 2014/05/21 15:23:31
-}

module HasKAL.FrameUtils.PickUpFileName (
quicksort
,pickUpFileName
,pickUpFileNameinFile
,pickUpFileNameinoutFile
) where


import Data.Char
import System.IO
import System.IO.Unsafe
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Map as M 
import Data.List.Split 

{-
main = do 
-- pickUpFileName    
     handle <- openFile "GEOframecache.dat" ReadMode
     contents <- hGetContents handle
     let contentsList = lines contents

     mapM putStrLn $ pickUpFileName 993597840 993598000 contentsList

--pickUpFileNameinFile
     mapM putStrLn $ P.pickUpFileNameinFile 993597840 993598000 "GEOframecache.dat"

--pickUpFileNameinoutFile
     P.pickUpFileNameinoutFile 993597840 993598000 "GEOframecache.dat" 

--   KAGRA's FileName is "K-K1_C-1083981344-32.gwf" (one example)
-}

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
     let smallOrEqual = [a | a <- xs, a <= x]
     	 larger = [a | a <- xs, a > x]
     in quicksort smallOrEqual ++ [x] ++ quicksort larger


pickUpFileName :: Integer -> Integer -> [String] -> [String]
pickUpFileName gpsTimeStart gpsTimeFinish contentsList = do
     let extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer 
       	 contentsVec = V.fromList contentsList --(quicksort contentsList)
       	 gpstimeList :: [Integer]
       	 gpstimeList = map extractstartGPStime contentsList --(quicksort contentsList) 
       	 time2index :: M.Map Integer Int
       	 time2index = M.fromList $ zip gpstimeList [1..]		  
	 startIndex = case M.lookupLE gpsTimeStart time2index of
     	                   Nothing -> -1
		           Just (_,i) -> i
	 finishIndex = case M.lookupLE gpsTimeFinish time2index of
                            Nothing -> -1
	     	     	    Just (_,j) -> j

     case (startIndex,finishIndex) of (-1,_) -> ["Nothing"]
          			      (_,-1) -> ["Nothing"]
				      (_,_)  -> drop (startIndex -1) $ take (finishIndex) contentsList --(quicksort contentsList)


pickUpFileNameinFile :: Integer -> Integer -> String -> [String]
pickUpFileNameinFile gpsTimeStart gpsTimeFinish fileName = pickUpFileName gpsTimeStart gpsTimeFinish contentsList
     where contentsList = lines $ unsafePerformIO $ readFile fileName


pickUpFileNameinoutFile :: Integer -> Integer -> String -> IO ()
pickUpFileNameinoutFile gpsTimeStart gpsTimeFinish fileName = writeFile "tmpCachedFrameFile.lst" contentsList
     where contentsList = unlines $ pickUpFileNameinFile gpsTimeStart gpsTimeFinish fileName











------------ MEMO ------------

{-
pickUpFileNameinFile :: Integer -> Integer -> String -> IO String
pickUpFileNameinFile gpsTimeStart gpsTimeFinish fileName = do
     handle <- openFile fileName ReadMode	
     contents <- hGetContents handle
     let contentsList = lines contents	    
     	 extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer
         contentsVec = V.fromList contentsList
         gpstimeList :: [Integer]
         gpstimeList = map extractstartGPStime contentsList
         time2index :: M.Map Integer Int
         time2index = M.fromList $ zip gpstimeList [1..]
	 startIndex = case M.lookupLE gpsTimeStart time2index of
                           Nothing -> -1
                           Just (_,i) -> i
	 finishIndex = case M.lookupLE gpsTimeFinish time2index of
                            Nothing -> -1
                            Just (_,j) -> j

     asano <- case (startIndex,finishIndex) of (-1,_) -> return "Nothing"
         	       			       (_,-1) -> return "Nothing"
					       (_,_)  -> return $ unlines $ drop (startIndex -1) $ take (finishIndex) contentsList
     hClose handle
     return asano


pickUpFileNameinoutFile :: Integer -> Integer -> String -> IO ()
pickUpFileNameinoutFile gpsTimeStart gpsTimeFinish fileName = do
     handle <- openFile fileName ReadMode	
     contents <- hGetContents handle
     let contentsList = lines contents	    
     	 extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer
         contentsVec = V.fromList contentsList
         gpstimeList :: [Integer]
         gpstimeList = map extractstartGPStime contentsList
         time2index :: M.Map Integer Int
         time2index = M.fromList $ zip gpstimeList [1..]
	 startIndex = case M.lookupLE gpsTimeStart time2index of
                           Nothing -> -1
                           Just (_,i) -> i
	 finishIndex = case M.lookupLE gpsTimeFinish time2index of
                            Nothing -> -1
                            Just (_,j) -> j

     asano <- case (startIndex,finishIndex) of (-1,_) -> return "Nothing"
         	       			       (_,-1) -> return "Nothing"
					       (_,_)  -> return $ unlines $ drop (startIndex -1) $ take (finishIndex) contentsList
     hClose handle

     withFile "tmpCachedFrameFile.lst" WriteMode $ \handle -> do
     hPutStrLn handle asano

-}


