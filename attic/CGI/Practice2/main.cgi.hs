

-- import Debug.Trace (trace)

import Network.CGI
import Control.Monad (liftM, forM)
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (fromList, length)
import System.Directory (doesFileExist)

import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV)
import HasKAL.SpectrumUtils.Function (getSpectrum)
import HasKAL.PlotUtils.HROOT.PlotGraph (LogOption(..), PlotTypeOption(..), ColorOpt(..), plotV, oPlotV)
import HasKAL.PlotUtils.HROOT.PlotGraph3D (LogOption(..), PlotTypeOption3D(..), spectrogramM)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon (rayleighMonV)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon (FitMethod(..), studentRayleighMonV)

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

process :: ParamCGI -> IO [(String, String, [String])]
process params = do
  let gps' = fromJust $ gps params
      duration' = duration params
      fmin' = fmin params
      fmax' = fmax params
      channel1' = channel1 params
      monitors' = monitors params
  forM channel1' $ \ch -> do
    datMaybe <- kagraDataGet (read gps') (read duration') ch
    case datMaybe of
     Nothing -> return ("Can't find file or channel", ch, []) -- データが無ければメッセージを返す
     _ -> do
       fs <- liftM fromJust $ (`getSamplingFrequency` ch) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch)
       let dat = fromJust datMaybe
           tvec = V.fromList $ take (V.length dat) [0,1/fs..]
           snf = gwpsdV dat (truncate fs) fs
           hfs = gwspectrogramV 0 (truncate fs) fs dat
           fRange = (read fmin', read fmax')
       files <- forM monitors' $ \mon -> do
         let pngfile = pngDir++ch++"_"++gps'++"_"++mon++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
         pngExist <- doesFileExist pngfile
         case pngExist of
          True -> return () -- 既にPNGがあれば何もしない
          False -> do
            case mon of
             "TS" -> do
               -- let dat' = filter dat
               plotV Linear Line 1 BLUE ("s", "") 0.05 ("Time Series: "++ch++" GPS="++gps') pngfile ((0,0),(0,0)) (tvec, dat)
             "PSD" -> do
               plotV LogXY Line 1 BLUE ("Hz", "/rHz") 0.05 ("Spectrum: "++ch++" GPS="++gps') pngfile (fRange,(0,0)) snf
             "SPE" -> do
               spectrogramM LogZ COLZ "/rHz" ("Spectrogram: "++ch++" GPS="++gps') pngfile ((0,0),fRange) hfs
             "RM" -> do
               let qv = rayleighMonV [0.5, 0.9, 0.95, 0.99] fs (truncate fs) 16 snf hfs
               oPlotV Linear LinePoint 1 [RED, RED, BLUE, BLUE, PINK, PINK, GREEN, GREEN] ("Hz", "Normalized Noise Lv.")
                 0.05 ("RayleighMon: "++ch++" GPS="++gps') pngfile (fRange, (0,6)) (concat $ map (\(x,y)->[x,y]) qv)
             "SRM" -> do
               let size = truncate (read duration') - 1
                   nus = studentRayleighMonV (QUANT 0.95) fs (truncate fs) size size 16 snf hfs
               plotV Linear LinePoint 1 BLUE ("Hz", "nu") 0.05 ("StudentRayleighMon: "++ch++" GPS="++gps') pngfile
                 (fRange,(0,0)) (getSpectrum 0 nus)
         return pngfile
       return ("", ch, files)

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\">",
          dateForm params,
          channelForm params [Multi],
          paramForm,
          monitorForm Multi [(True, "TS", "Time Series"),
                             (True, "PSD", "Spectrum"),
                             (True, "SPE", "Spectrogram"),
                             (False, "RM", "RayleighMon"),
                             (False, "SRM", "StudentRayleighMon")],
          "<div><input type=\"submit\" value=\"plot view\" /></div>",
          "</form>"]

resultPage :: ParamCGI -> [(Message, String, [String])] -> String
resultPage params filenames = resultFrame params $ genePngTable filenames 

{--
Channel名
  K1:PEM-EX_MAG_X_FLOOR_GpsTime_MonName_Params.png
--}
