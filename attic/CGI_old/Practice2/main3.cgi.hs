
import Network.CGI
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (maximum)
import Control.Monad (forM, liftM)

import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation (takeCorrelationV)
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import Function


main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  params <- getInputParams
  str <- liftIO $ fork params
  output $ str


fork :: ParamCGI -> IO String
fork params = do
  nowGps <- getCurrentGps
  case (gps params, channel1 params, monitors params) of
   (Nothing, _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (Just "", _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (_, [], _)      -> return $ inputForm $ updateMsg "unselected channel" params
   (_, _, [])      -> return $ inputForm $ updateMsg "unselected monitor" params
   (Just x,  _, _)  -> do
     fnames <- process params
     return $ resultPage params fnames

process :: ParamCGI -> IO [(Message, String, [String])]
process params = do
  let gps' = fromJust $ gps params
      duration' = duration params
      fmin' = fmin params
      fmax' = fmax params
      chs = channel1 params
      mon = head $ monitors params
  forM chs $ \ch1 -> do
    datMaybe <- kagraDataGet (read gps') (read duration') ch1
    case datMaybe of
     Nothing -> return ("Can't find channel", ch1, [""])
     _       -> do
       fs1 <- liftM fromJust $ (`getSamplingFrequency` ch1) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch1)
       vals <- forM chs $ \ch2 -> do
         datMaybe2 <- kagraDataGet (read gps') (read duration') ch2
         case (datMaybe2, ch1/=ch2) of
          (Nothing, _) -> return "0"
          (_, False)   -> return "1"
          (_, True)    -> do
            fs2 <- liftM fromJust $ (`getSamplingFrequency` ch2) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch2)
            return $ show $ V.maximum $ takeCorrelationV (read $ mon) (fromJust datMaybe) (fromJust datMaybe2) 16
       return ("", ch1, vals)
             

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\">",
          dateForm params,
          channelForm params [Multi],
          paramForm,
          monitorForm Single [(True, "Peason", "Peason Correlation")
                             ,(False, "MIC", "<s>MIC</s>")
                             ],
          "<div><input type=\"submit\" value=\"plot view\" /></div>",
          "</form>"]

resultPage :: ParamCGI -> [(Message, String, [String])] -> String
resultPage params result = resultFrame params $ geneChMap result
