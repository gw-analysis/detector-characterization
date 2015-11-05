
import Debug.Trace(trace)
import Network.CGI
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (maximum)
import Control.Monad (forM, liftM)
import Data.List (isInfixOf)

import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getChannelList)
import HasKAL.MonitorUtils.CoherenceMon.Function (hBruco)
import HasKAL.WebUtils.CGI.Function


main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  params <- getInputParams
  str <- liftIO $ fork params
  output $ str

fork :: ParamCGI -> IO String
fork params = do
  nowGps <- return $ show 1120543424 -- getCurrentGps
  case (gps params, channel1 params, monitors params) of
   (Nothing, _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (Just "", _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (_, [], _)      -> return $ "<html><body><h1>unselected channel</h1></body></html>"
   (_, _, [])      -> return $ "<html><body><h1>unselected monitor</h1></body></html>"
   (Just x,  _, _)  -> do
     fnames <- process params
     return $ resultPage params fnames
   -- (_, [], []) -> do
   --   let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] [] $ defaultMon ["COH"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames
   -- (_, [], _) -> do
   --   let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] [] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames
   -- (_, _, []) ->  do
   --   let params' = defaultMon ["COH"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames
   -- (Just x,  _, _) -> do
   --   fnames <- process params
   --   return $ resultPage params fnames

process :: ParamCGI -> IO [(Double, [(Double, String)])]
process params = do
  let gps' = fromJust $ gps params
      duration' = duration params
      fmin' = fmin params
      fmax' = fmax params
      ch1 = head $ channel1 params
      mon = head $ monitors params
  fileMaybe <- kagraDataFind (read gps') (read duration') ch1
  case fileMaybe of
   Nothing -> return [((-1), [(0.0, "")])]
   _       -> do
     mbFiles <- kagraDataFind (read gps') (read duration') ch1
     datMaybe1 <- kagraDataGet (read gps') (read duration') ch1
     mbFs1 <- getSamplingFrequency (head $ fromJust mbFiles) ch1
     chlist <- liftM fromJust $ getChannelList $ (fromJust fileMaybe)!!0
     let chlist' = filter (/=ch1) $ filter (isInfixOf "K1:") $ map fst chlist
     mbFs2 <- mapM (getSamplingFrequency (head $ fromJust mbFiles)) chlist'
     datMaybe2 <- mapM (kagraDataGet (read gps') (read duration')) $ filter (/=ch1) chlist'
     return $ hBruco 1 (fromJust mbFs1, fromJust datMaybe1, ch1) $ zip3 (map fromJust mbFs2) (map fromJust datMaybe2) chlist'
            
inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          (dateForm params),
          channelForm params [Multi],
          paramForm [],
          monitorForm Single [(True, COH, "Bruco")],
          "<div><input type=\"submit\" value=\"plot view\" /></div>",
          "</form>"]

resultPage :: ParamCGI -> [(Double, [(Double, String)])] -> String
resultPage params result = resultFrame params (geneRankTable params result)
