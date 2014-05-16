{-
File name : PickUpFileName.hs
Author : Mitsuhiro Asano
-}

module PickUpFileName (pickUpFileName) where

import Data.Char
import System.IO
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Map as M 
import Data.List.Split 

{-
main = do 
     handle <- openFile "GEOframecache.dat" ReadMode
     contents <- hGetContents handle
     let contentsList = lines contents

     mapM putStrLn $ pickUpFileName "993597840" "993598000" contentsList

--KAGRA's FileName is "K-K1_C-1083981344-32.gwf" (one example)

-}

pickUpFileName :: String -> String -> [String] -> [String]
pickUpFileName gpsTimeStart gpsTimeFinish contentsList = do
     let extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer 
       	 contentsVec = V.fromList contentsList
       	 gpstimeList :: [Integer]
       	 gpstimeList = map extractstartGPStime contentsList 
       	 time2index :: M.Map Integer Int
       	 time2index = M.fromList $ zip gpstimeList [1..]    	  
	 startIndex = case M.lookupLE (read gpsTimeStart) time2index of
     	  	      	   Nothing -> -1
			   Just (_,i) -> i
	 finishIndex = case M.lookupLE (read gpsTimeFinish) time2index of
      	     	       	    Nothing -> -1
			    Just (_,j) -> j

     case (startIndex,finishIndex) of (-1,_) -> ["Nothing"]
	     		    	      (_,-1) -> ["Nothing"]
				      (_,_)  -> drop (startIndex -1) $ take (finishIndex) contentsList
--				      (_,_)  -> take (finishIndex - startIndex +1) $ drop (startIndex -1) contentsList

