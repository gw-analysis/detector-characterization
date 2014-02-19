import Data.Char
import System.IO
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Map as M 
import Data.List.Split 

main = do 
     withFile "GEOframecache.dat" ReadMode $ \handle -> do
          contents <- hGetContents handle
	  let contentsList = lines contents
	      extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Integer 
	      contentsVec = V.fromList contentsList
	      gpstimeList :: [Integer]
	      gpstimeList = map extractstartGPStime contentsList 
	      time2index :: M.Map Integer Int
	      time2index = M.fromList $ zip gpstimeList [0..]    	  

	  --print $ contentsVec ! 0    
	  putStrLn "Input GPStime"
	  gpstimestr <- getLine
	  let gpstime = read gpstimestr
--	      gpstime60 = div gpstime 60 * 60
--	  let function :: Integer -> String
--	      function gpstime = path ++ head ++ show gpstime60 ++ tail
--	      	       where path = "/opt/workspace/GEO/9935/"
--		       	     head = "G-G1_RDS_C01_L3-"
--			     tail = "-60.gwf"
          print $ M.lookupLE gpstime time2index
	  case M.lookupLE gpstime time2index of 
	       Nothing -> return ()
	       Just (_,i) -> 	
	       	    putStr (contentsVec ! i)

