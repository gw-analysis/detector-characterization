
import Network.CGI
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (fromList, length, take)
import Control.Monad (liftM, forM)
import System.Directory (doesFileExist)

import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.MonitorUtils.CoherenceMon.Function (coherenceMon)
import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation (takeCorrelationV)
import HasKAL.PlotUtils.HROOT.PlotGraph (LogOption(..), PlotTypeOption(..), ColorOpt(..), plotV)
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
  -- let params = updateGps nowGps params''
  case (gps params, channel1 params, channel2 params, monitors params) of
   -- (Nothing, _, _, _) -> return $ inputDateForm $ updateMsg "" $ updateGps nowGps params
   -- (Just "", _, _, _) -> return $ inputDateForm $ updateMsg "" $ updateGps nowGps params
   (Nothing, _, _, _) -> do
     let params' = updateGps nowGps $ defaultMon ["COH"] $ defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] ["K1:PEM-EX_ACC_NO2_Y_FLOOR"] params
     fnames <- process params'
     return $ resultPage params' fnames
   (Just "", _, _, _) -> do
     let params' = updateGps nowGps $ defaultMon ["COH"] $ defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] ["K1:PEM-EX_ACC_NO2_Y_FLOOR"] params
     fnames <- process params'
     return $ resultPage params' fnames
   (_, [], [], []) -> do
     let params' = defaultMon ["COH"] $ defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] ["K1:PEM-EX_ACC_NO2_Y_FLOOR"] params
     fnames <- process params'
     return $ resultPage params' fnames 
   (_, [], [], _) -> do
     let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] ["K1:PEM-EX_ACC_NO2_Y_FLOOR"] params
     fnames <- process params'
     return $ resultPage params' fnames 
   (_, [], _, _)  -> return $ inputDateForm $ updateMsg "unselected channel1" params
   (_, _, [], _)  -> return $ inputDateForm $ updateMsg "unselected channel2" params   
   (_, _, _, [])  -> do
     let params' = defaultMon ["COH"] params
     fnames <- process params'
     return $ resultPage params' fnames 
   (Just x,  _, _, _)  -> do
     fnames <- process params
     return $ resultPage params fnames

defaultChs :: [String] -> [String] -> ParamCGI -> ParamCGI
defaultChs defch1 defch2 params =
  ParamCGI { script = script params
           , message = message params
           , files = files params
           , lstfile = lstfile params
           , chlist = chlist params
           , gps = gps params
           , locale = locale params
           , channel1 = defch1
           , channel2 = defch2
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

process :: ParamCGI -> IO [(Message, String, [String])]
process params = do
  let gps' = fromJust $ gps params
      duration' = duration params
      fmin' = fmin params
      fmax' = fmax params
      ch1 = head $ channel1 params
      chs = channel2 params
      monitors' = monitors params
  datMaybe <- kagraDataGet (read gps') (read duration') ch1
  case datMaybe of
   Nothing -> return [("Can't find file or channel1", ch1, [])] -- データが無ければメッセージを返す
   _ -> do
     fs1 <- liftM fromJust $ (`getSamplingFrequency` ch1) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch1)
     let dat1 = fromJust datMaybe
         snf1 = gwpsdV dat1 (truncate fs1) fs1
         refpng = pngDir++ch1++"_"++gps'++"_"++"REFSPE"++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
     refExist <- doesFileExist refpng
     case refExist of
      True -> return ()
      False -> plotV LogY Line 1 BLUE ("Hz", "/Hz") 0.05 ("Spectrum: "++ch1++" GPS="++gps') refpng
               ((read fmin',read fmax'),(0,0)) $ (\(x, y) -> (V.take (V.length x `div`2) x, V.take (V.length x `div`2) y)) snf1
     result <- forM chs $ \ch2 -> do
       datMaybe2 <- kagraDataGet (read gps') (read duration') ch2
       case datMaybe2 of
        Nothing -> return ("Can't find file or channel", ch2, []) -- データが無ければメッセージを返す
        _ -> do
          fs2 <- liftM fromJust $ (`getSamplingFrequency` ch2) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch2)
          let dat2 = fromJust datMaybe2
              tvec = V.fromList [0,1/fs1..(fromIntegral $ V.length dat2-1)/fs1]
              snf2 = gwpsdV dat2 (truncate fs2) fs2
              refpng2 = pngDir++ch2++"_"++gps'++"_"++"REFSPE"++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
          refExist2 <- doesFileExist refpng2
          case refExist2 of
           True -> return () -- 既にPNGがあれば何もしない
           False -> plotV LogY Line 1 BLUE ("Hz", "/Hz") 0.05 ("Spectrum: "++ch2++" GPS="++gps') refpng2
                    ((read fmin',read fmax'),(0,0)) $ (\(x, y) -> (V.take (V.length x `div`2) x, V.take (V.length x `div`2) y)) snf2
          files <- forM monitors' $ \mon -> do
            let pngfile = pngDir++ch1++"-vs-"++ch2++"_"++gps'++"_"++mon++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
            pngExist <- doesFileExist pngfile
            case pngExist of
             True -> return () -- 既にPNGがあれば何もしない
             False -> do
               case mon of
                "COH" -> do
                  let coh = coherenceMon 1 fs1 fs2 dat1 dat2 -- length of FFT = 1 second
                  plotV Linear Line 1 BLUE ("Hz", "|Coh(f)|^2") 0.05 ("Coherence: "++ch1++" vs "++ch2++" GPS="++gps')
                    pngfile ((read fmin',read fmax'),(-0.05,1.05)) coh
                "Peason" -> do
                  let cor = takeCorrelationV (read mon) dat1 dat2 16
                  plotV Linear LinePoint 1 BLUE ("s", "correlation") 0.05 ("Peason: "++ch1++" vs "++ch2++" GPS="++gps')
                    pngfile ((0,0),(0,0)) (tvec, cor)
                "MIC" -> do
                  return () -- 未実装
                  -- let cor = takeCorrelationV (read mon) dat1 dat2 16
                  -- plotV Linear LinePoint 1 BLUE ("s", "correlation") 0.05 ("MIC: "++ch1++" vs "++ch2++" GPS="++gps')
                  --   pngfile ((0,0),(0,0)) (tvec, cor)
            return pngfile
          return ("", ch2, refpng2:files)
     return $ ("", "Reference: "++ch1, [refpng]):result

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

resultPage :: ParamCGI -> [(Message, String, [String])] -> String
resultPage params filenames = resultFrame' params (genePngTable filenames) (inputForm params)

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<br><form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          "<div style=\"background: #EFEFEF; border: 1px solid #CC0000; height:100％;",
          "padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px;\">", 
          timeForm' params,
          "<div style=\"float:left; margin-right:50\">", channelForm params [Single, Multi], "</div>",
          "<div style=\"float:left;\">", paramForm [], "</div>",
          "<div style=\"clear:both;\"></div><br>", 
          monitorForm Multi [(True, COH, "CoherenceMon")
                            ,(False, Peason, "Peason Correlation")
                            ,(False, MIC, "<s>MIC</s>")
                            ],
          "<div><input type=\"submit\" value=\"plot view\" /></div>",
          "</div></form>"]

