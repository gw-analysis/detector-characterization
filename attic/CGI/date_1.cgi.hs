
{-- for Debug --}
import Debug.Trace (trace)

import Control.Monad (liftM, forM)
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (fromList, length)
import Network.CGI (CGI, CGIResult, liftIO, output, runCGI, handleErrors)
import System.Directory (doesFileExist)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraDataFind)
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils (nha, formatNHA, butterBandPass)
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
-- import HasKAL.MonitorUtils.NoiseFloorMon.NoiseFloorMon (getNoiseFloorStatusV, makeNFMparam, estimateThreshold)
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMon)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon (rayleighMonWaveData)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon (studentRayleighMonWaveData)
import HasKAL.MonitorUtils.SensMon.SensMon (runSensMon)
import HasKAL.PlotUtils.HROOT.PlotGraph (LogOption(..), PlotTypeOption(..), ColorOpt(..), plotV, oPlotV)
import HasKAL.PlotUtils.HROOT.PlotGraph3D (LogOption(..), PlotTypeOption3D(..), histgram2dM)
import HasKAL.SpectrumUtils.Function (getSpectrum, mapSpectrum, mapSpectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDWaveData, gwspectrogramWaveData)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (waveData2TimeSeries)
import HasKAL.WebUtils.CGI.Function (Message, ParamCGI(..), MonitorType(..), MultiSelect(..)
                                    ,pngDir ,getInputParams, updateMsg, updateGps, genePngTable
                                    ,inputFrame, resultFrame, dateForm, channelForm, paramForm, monitorForm)
import SampleChannel

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  params <- getInputParams defaultChs
  str <- liftIO $ fork params
  output $ str

fork :: ParamCGI -> IO String
fork params = do
  nowGps <- getCurrentGps
  case (gps params, channel1 params, monitors params) of
   (Nothing, _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (Just "", _, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (_, [], _)      -> return $ "<html><body><h1>Any channel is not selected</h1></body></html>"
   (_, _, [])      -> return $ "<html><body><h1>Any monitor is not selected</h1></body></html>"
   (Just x,  _, _)  -> do
     fnames <- process params
     return $ resultPage params fnames
   -- (_, [], []) -> do
   --   let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] [] $ defaultMon ["TS"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames
   -- (_, [], _) -> do
   --   let params' = defaultChs ["K1:PEM-EX_ACC_NO2_X_FLOOR"] [] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames
   -- (_, _, []) ->  do
   --   let params' = defaultMon ["TS"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames
   -- (Just x,  _, _) -> do
   --   fnames <- process params
   --   return $ resultPage params fnames

process :: ParamCGI -> IO [(String, String, [String])]
process params = do
  {-- 入力パラメタ --}
  let gps' = read $ fromJust $ gps params
      duration' = read $ duration params
      channel1' = channel1 params
      monitors' = monitors params
      fminS = fmin params
      fmaxS = fmax params
  {-- データ取得 --}
  forM channel1' $ \ch -> do
    mbWd <- kagraWaveDataGetC gps' (truncate duration') ch
    case mbWd of
     Nothing -> return ("ERROR: Can't find file or channel", ch, [])
     Just (wd:wds) -> do
       unit <- (\x -> safeGetUnitY x ch) =<<
               liftM (head.fromJust) (kagraDataFind (fromIntegral gps') (truncate duration') ch) -- データがあったのでファイルは必ずある
       {-- 各モニタ共通パラメタ --}
       let oFile cName mName = pngDir++cName++"_"++(show gps')++"-"++mName++"_"++(show duration')++"_fl"++fminS++"_fh"++fmaxS++".png"
           labelTime = "time [s] since GPS="++(show gps')
           labelFreq = "frequency [Hz] (GPS="++(show gps')++")"
           fmin' | read fminS >= samplingFrequency wd = 0.0
                 | otherwise = max 0.0 . read $ fminS
           fmax' | read fmaxS <= 0.0 = samplingFrequency wd
                 | otherwise = min (samplingFrequency wd) . read $ fmaxS
       {-- 各モニタ処理 --}
       filesL <- forM monitors' $ \mon -> do
         case mon of -- 1モニタ複数プロット
          {-- NHA Plot --}
          "NHA" -> do 
            let pngfile1 = oFile ch $ mon++"-A"
                pngfile2 = oFile ch $ mon++"-F"
            pngExist <- doesFileExist pngfile1
            case pngExist of
             True -> return [pngfile1, pngfile2] -- 既にPNGがあれば何もしない
             False -> do
               let eiDat = butterBandPass (gwdata wd) (samplingFrequency wd) fmin' fmax' 6
               case eiDat of
                Left msg -> return ["ERROR: "++msg] -- フィルタエラーならメッセージを返す
                Right dat' -> do
                  let output = formatNHA $ nha dat' (samplingFrequency wd) 4 1024 256 0 (V.length dat') 0.0
                  oPlotV Linear [Point] 1 [RED, BLUE] (labelTime, unitBracket "amplitude" unit) 0.05
                    ("NHA: "++ch) pngfile1 ((0,0),(0,0)) $ (output!!0)
                  oPlotV Linear [Point] 1 [RED, BLUE] (labelTime, "frequency [Hz]") 0.05 
                    ("NHA: "++ch) pngfile2 ((0,0),(fmin',fmax')) $ (output!!1)
                  return [pngfile1, pngfile2]
          -- ここから1モニタ1プロット
          _ -> do 
            let pngfile = oFile ch mon
            pngExist <- doesFileExist pngfile
            case (pngExist, mon) of
             (True, _) -> return () -- 既にPNGがあれば何もしない
             {-- Time Series Plot --}
             (_, "TS") -> do
               let hts = map (waveData2TimeSeries (gps',0)) (wd:wds)
               oPlotV Linear [Line] 1 (replicate (length hts) BLUE) (labelTime, unitBracket "amplitude" unit) 0.05
                 ("Time Series: "++ch) pngfile ((0,duration'),(0,0)) hts
             {-- Spectrum Plot --}
             (_, "PSD") -> do
               let snf = mapSpectrum sqrt $ gwOnesidedPSDWaveData duration' wd
               plotV LogXY Line 1 BLUE (labelFreq, "ASD ["++unit++"/rHz]") 0.05
                 ("Spectrum: "++ch) pngfile ((fmin',fmax'),(0,0)) snf
             {-- Spectrogram Plot --}
             (_, "SPE") -> do
               let hfs = mapSpectrogram sqrt $ gwspectrogramWaveData 0 (min 1 duration') wd
               histgram2dM LogZ COLZ (labelTime, "frequency [Hz]", "ASD ["++unit++"/rHz]")
                 ("Spectrogram: "++ch) pngfile ((0,0),(fmin',fmax')) hfs
             {-- Rayleigh Monitor --}
             (_, "RM") -> do
               let qval = concatMap (\(x,y)->[x,y]) $ rayleighMonWaveData [0.5, 0.95, 0.99] (min 1 duration') 16 wd wd
               oPlotV Linear (take 6 $ cycle [LinePoint, Line]) 1 (concatMap (replicate 2) [RED, BLUE, PINK]) (labelFreq, "Normalized Noise Lv.") 0.05
                 ("RayleighMon: "++ch) pngfile ((fmin',fmax'), (0,0)) qval
             {-- Student-Rayleigh Monitor --}
             (_, "SRM") -> do
               let nus = getSpectrum 0 $ studentRayleighMonWaveData 0.95 (min 1 duration') duration' duration' 16 wd wd
               plotV Linear LinePoint 1 BLUE (labelFreq, "#nu") 0.05 
                 ("StudentRayleighMon: "++ch) pngfile ((fmin',fmax'),(0,0)) nus
             {-- RMS Monitor --}
             (_, "RMS") -> do
               let rms = rmsMon (V.length (gwdata wd) `div` 32) (samplingFrequency wd) (gwdata wd) [(fmin', fmax')]
               oPlotV Linear [LinePoint] 1 [] (labelTime, unitBracket "RMS" unit) 0.05
                 ("RMSMon: "++ch) pngfile ((0,0),(0,0)) rms
             {-- Sensitivity Monitor --}
             (_, "Sens") -> do
               let (sens, _) = runSensMon (gwdata wd) (samplingFrequency wd) (truncate (samplingFrequency wd))
               histgram2dM LogXZ COLZ (labelFreq, "ASD [log("++unit++"/rHz)]", "yield")
                 ("SensMon: "++ch) pngfile ((0,0),(fmin',fmax')) sens
             {-- Glitch Monitor --}
             (_, "Glitch") -> do
               return ()
             {-- Line Finder --}
             (_, "LineFind") -> do
               return ()
             {-- Noise Floor Monitor --}
             (_, "NFM") -> do
               return ()
            return [pngfile] {-- 各種モニタ処理終了 --}
       return (show (samplingFrequency wd), ch, concat filesL)

unitBracket :: String -> String -> String
unitBracket x "" = x
unitBracket x y  = x++" ["++y++"]"

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          (dateForm params),
          channelForm params [Multi],
          paramForm [NHA],
          monitorForm Multi [(False, TS, "Time Series"),
                             (False, PSD, "Spectrum"),
                             (False, SPE, "Spectrogram"),
                             (False, RM, "RayleighMon"),
                             (False, SRM, "StudentRayleighMon"),
                             (False, RMS, "RMSMon"),
                             (False, Sens, "SensMon"),
                             (False, Glitch, "<s>GlitchMon</s>"),
                             (False, LineFind, "<s>LineFinder</s>"),
                             (False, NHA, "LineTracking"){--,
                              (False, NFM, "<s>NoiseFloorMon</s>") --}
                             ],
          "<br><center>",
          "<div style=\"padding:15px 15px;",
          "background-color:coral;width:80px;border-radius:20px;\">",
          "<input type=\"submit\" value=\"plot view\" style=\"font-size:16px\"></div>",
          "</center>",
          "</form>"]

resultPage :: ParamCGI -> [(Message, String, [String])] -> String
resultPage params filenames = resultFrame params (genePngTable filenames)

{--
Channel名
  K1:PEM-EX_MAG_X_FLOOR_GpsTime_MonName_Params.png
--}
