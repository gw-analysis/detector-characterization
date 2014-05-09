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

     putStrLn $ pickUpFileName "993597840" contentsList
-}

pickUpFileName :: String -> [String] -> String
pickUpFileName gpsTime contentsList = do
     let extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer 
       	 contentsVec = V.fromList contentsList
       	 gpstimeList :: [Integer]
       	 gpstimeList = map extractstartGPStime contentsList 
       	 time2index :: M.Map Integer Int
       	 time2index = M.fromList $ zip gpstimeList [0..]    	  

     case M.lookupLE (read gpsTime) time2index of
     	  Nothing -> "Nothing"
	  Just (_,i) -> (contentsVec ! i)





