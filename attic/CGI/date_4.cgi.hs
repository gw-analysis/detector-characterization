
import Debug.Trace(trace)
import Network.CGI
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (maximum)
import Control.Monad (forM, liftM)
import Data.List (isInfixOf)

import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
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
      chlist = filter (/=ch1) $ channel2 params
      mon = head $ monitors params
  datMaybe1 <- kagraDataGet (read gps') (read duration') ch1
  case datMaybe1 of
   Nothing -> return [((0), [(0.0, "")])]
   Just dat1 -> do
     fs1 <- liftM fromJust $ (`getSamplingFrequency` ch1) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch1)
     let chlist' = case chlist==[] of
                    True -> [ch1]
                    False -> take 20 chlist -- 最大20chで制限
     dat2List <- forM chlist' $ \ch2 -> do
       datMaybe2 <- kagraDataGet (read gps') (read duration') ch2
       mbFs2 <- (`getSamplingFrequency` ch2) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch2)
       return (mbFs2, datMaybe2, ch2)
     return $ hBruco 1 (fs1, dat1, ch1) $
       map (\(x, y, z) -> (fromJust x, fromJust y, z)) $ filter ((/=Nothing).(\(x,_,_) -> x)) dat2List
            
inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          (dateForm params),
          channelForm params [Single, Multi],
          paramForm [],
          monitorForm Single [(True, COH, "Bruco")],
          "<br><center>",
          "<div style=\"padding:15px 15px;",
          "background-color:coral;width:80px;border-radius:20px;\">",
          "<input type=\"submit\" value=\"plot view\" style=\"font-size:16px\"></div>",
          "</center>",
          "</form>"]

resultPage :: ParamCGI -> [(Double, [(Double, String)])] -> String
resultPage params result = resultFrame params (geneRankTable params result)
