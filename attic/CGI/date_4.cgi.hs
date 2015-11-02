
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
   (Nothing, _, _) -> return $ inputDateForm $ updateMsg "" $ updateGps nowGps params
   (Just "", _, _) -> return $ inputDateForm $ updateMsg "" $ updateGps nowGps params
   (_, [], []) -> do
     let params' = defaultMon ["COH"] $ defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] params
     fnames <- process params'
     return $ resultPage params' fnames
   (_, [], _) -> do
     let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] params
     fnames <- process params'
     return $ resultPage params' fnames
   (_, _, []) ->  do
     let params' = defaultMon ["COH"] params
     fnames <- process params'
     return $ resultPage params' fnames
   (Just x,  _, _) -> do
     fnames <- process params
     return $ resultPage params fnames

defaultChs :: [String] -> ParamCGI -> ParamCGI
defaultChs defch params =
  ParamCGI { script = script params
           , message = message params
           , files = files params
           , lstfile = lstfile params
           , chlist = chlist params
           , gps = gps params
           , locale = locale params
           , channel1 = defch
           , channel2 = channel2 params
           , monitors = monitors params
           , duration = duration params
           , fmin = fmin params
           , fmax = fmax params
           }

defaultMon :: [String] -> ParamCGI -> ParamCGI
defaultMon defmon params =
  ParamCGI { script = script params
           , message = message params
           , files = files params
           , lstfile = lstfile params
           , chlist = chlist params
           , gps = gps params
           , locale = locale params
           , channel1 = channel1 params
           , channel2 = channel2 params
           , monitors = defmon
           , duration = duration params
           , fmin = fmin params
           , fmax = fmax params
           }

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
     datMaybe1 <- kagraDataGet (read gps') (read duration') ch1
     chlist <- liftM fromJust $ getChannelList $ (fromJust fileMaybe)!!0
     let chlist' = filter (/=ch1) $ filter (isInfixOf "K1:") $ map fst chlist
     datMaybe2 <- mapM (kagraDataGet (read gps') (read duration')) $ filter (/=ch1) chlist'
     return $ hBruco 2048 2048 (fromJust datMaybe1, ch1) $ zip (map fromJust datMaybe2) chlist'
            
inputDateForm :: ParamCGI -> String
inputDateForm params = inputDateHeader dateformbody
  where dateformbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          (dateForm'' params),
          "<div><input type=\"submit\" value=\"plot view\" /></div>",
          "</form>"
          ]
        inputDateHeader x = concat [
          "<html><head><title>Date</title></head>",
          "<body><h1>Date</h1>",x,"</body></html>"
          ]

resultPage :: ParamCGI -> [(Double, [(Double, String)])] -> String
resultPage params result = resultFrame' params (geneRankTable params result) (inputForm params)

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<br><form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          "<div style=\"background: #EFEFEF; border: 1px solid #CC0000; height:100％;",
          "padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px;\">",
          "<form action=\"", (script params), "\" method=\"GET\">",
          timeForm' params,
          "<div style=\"float:left; margin-right:50\">", channelForm params [Single], "</div>",
          "<div style=\"float:left;\">", paramForm [], "</div>",
          "<div style=\"clear:both;\"></div><br>",
          monitorForm Single [(True, COH, "Bruco")],
          "<div><input type=\"submit\" value=\"plot view\" /></div>",
          "</div></form>"]

