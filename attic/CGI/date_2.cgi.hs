
import Network.CGI
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector.Storable as V (fromList, length, take)
import Control.Monad (liftM, forM)
import System.Directory (doesFileExist)

import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getUnitY)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV)
import HasKAL.SpectrumUtils.Function (mapSpectrum)
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
  case (gps params, channel1 params, channel2 params, monitors params) of
   (Nothing, _, _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (Just "", _, _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (_, [], _, _)      -> return $ "<html><body><h1>unselected channel1</h1></body></html>"
   (_, _, [], _)      -> return $ "<html><body><h1>unselected channel2</h1></body></html>"
   (_, _, _, [])      -> return $ "<html><body><h1>unselected monitor</h1></body></html>"
   (Just x,  _, _, _)  -> do
     fnames <- process params
     return $ resultPage params fnames
   -- (_, [], [], []) -> do
   --   let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] ["K1:PEM-EX_ACC_NO2_Y_FLOOR" defaultMon] $ defaultMon ["COH"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames 
   -- (_, [], [], _) -> do
   --   let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] ["K1:PEM-EX_ACC_NO2_Y_FLOOR"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames 
   -- (_, [], _, _)  -> return $ inputForm $ updateMsg "unselected channel1" params
   -- (_, _, [], _)  -> return $ inputForm $ updateMsg "unselected channel2" params   
   -- (_, _, _, [])  -> do
   --   let params' = defaultMon ["COH"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames 
   -- (Just x,  _, _, _)  -> do
   --   fnames <- process params
   --   return $ resultPage params fnames

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
     fname1 <- liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch1) -- データがあったのでファイルは必ずある
     fs1 <- liftM fromJust $ getSamplingFrequency fname1 ch1
     unit1 <- liftM (fromMaybe "") $ getUnitY fname1 ch1
     let dat1 = fromJust datMaybe
         snf1 = gwOnesidedPSDV dat1 (truncate fs1) fs1
         refpng = pngDir++ch1++"_"++gps'++"_"++"REFSPE"++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
     refExist <- doesFileExist refpng
     case refExist of
      True -> return ()
      False -> plotV LogY Line 1 BLACK ("frequency [Hz] (GPS="++gps'++")", unitBracket "ASD" (unit1++"/rHz")) 0.05 ("Spectrum: "++ch1) refpng ((read fmin',read fmax'),(0,0)) $ mapSpectrum sqrt snf1
     result <- forM chs $ \ch2 -> do
       datMaybe2 <- kagraDataGet (read gps') (read duration') ch2
       case datMaybe2 of
        Nothing -> return ("Can't find file or channel", ch2, []) -- データが無ければメッセージを返す
        _ -> do
          fname2 <- liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch2) -- データがあったのでファイルは必ずある
          fs2 <- liftM fromJust $ getSamplingFrequency fname2 ch2
          unit2 <- liftM (fromMaybe "") $ getUnitY fname2 ch2
          -- fs2 <- liftM fromJust $ (`getSamplingFrequency` ch2) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch2)
          let dat2 = fromJust datMaybe2
              snf2 = gwOnesidedPSDV dat2 (truncate fs2) fs2
              refpng2 = pngDir++ch2++"_"++gps'++"_"++"REFSPE"++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
          refExist2 <- doesFileExist refpng2
          case refExist2 of
           True -> return () -- 既にPNGがあれば何もしない
           False -> plotV LogY Line 1 BLACK ("frequency [Hz] (GPS="++gps'++")", unitBracket "ASD" (unit2++"/rHz")) 0.05 ("Spectrum: "++ch2) refpng2 ((read fmin',read fmax'),(0,0)) $ mapSpectrum sqrt snf2
          files <- forM monitors' $ \mon -> do
            let pngfile = pngDir++ch1++"-vs-"++ch2++"_"++gps'++"_"++mon++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
            pngExist <- doesFileExist pngfile
            case pngExist of
             True -> return () -- 既にPNGがあれば何もしない
             False -> do
               case mon of
                "COH" -> do
                  let coh = coherenceMon 1 fs1 fs2 dat1 dat2 -- length of FFT = 1 second
                  plotV Linear Line 1 BLUE ("frequency [Hz] (GPS="++gps'++")", "|Coh(f)|^2") 0.05 ("Coherence: "++ch1++" vs "++ch2)
                    pngfile ((read fmin',read fmax'),(-0.05,1.05)) coh
                "Peason" -> do
                  let cor = takeCorrelationV (read mon) dat1 dat2 16
                      tvec = V.fromList [0, 1/fs2..(fromIntegral $ V.length cor-1)/fs2]
                  plotV Linear LinePoint 1 BLUE ("time [s] since GPS="++gps', "correlation") 0.05 ("Peason: "++ch1++" vs "++ch2)
                    pngfile ((0,0),(0,0)) (tvec, cor)
                "MIC" -> do
                  return () -- 未実装
                  -- let cor = takeCorrelationV (read mon) dat1 dat2 16
                  --     tvec = V.fromList [0, 1/fs2..(fromIntegral $ V.length cor-1)/fs2]
                  -- plotV Linear LinePoint 1 BLUE ("time [s] since GPS="++gps', "correlation") 0.05 ("MIC: "++ch1++" vs "++ch2)
                  --   pngfile ((0,0),(0,0)) (tvec, cor)
            return pngfile
          return (show fs2, ch2, refpng2:files)
     return $ (show fs1, "Reference: "++ch1, [refpng]):result

unitBracket :: String -> String -> String
unitBracket x "" = x
unitBracket x y  = x++" ["++y++"]"

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          (dateForm params),
          channelForm params [Single, Multi],
          paramForm [],
          monitorForm Multi [(True, COH, "CoherenceMon")
                            ,(False, Peason, "Peason Correlation")
                            ,(False, MIC, "<s>MIC</s>")
                            ],
          "<br><center>",
          "<div style=\"padding:15px 15px;",
          "background-color:coral;width:80px;border-radius:20px;\">",
          "<input type=\"submit\" value=\"plot view\" style=\"font-size:16px\"></div>",
          "</center>",
          "</form>"]

resultPage :: ParamCGI -> [(Message, String, [String])] -> String
resultPage params filenames = resultFrame params (genePngTable filenames) 
