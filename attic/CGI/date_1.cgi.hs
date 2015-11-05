
{-- for Debug --}
import Debug.Trace (trace)
import qualified Data.Packed.Matrix as M (cols, rows)
import HasKAL.SpectrumUtils.Function (writeSpectrum, writeSpectrogram)

import Network.CGI
import Control.Monad (liftM, forM)
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (fromList, length, toList)
import System.Directory (doesFileExist)

import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV)
import HasKAL.SpectrumUtils.Function (getSpectrum, toSpectrum)
import HasKAL.PlotUtils.HROOT.PlotGraph (LogOption(..), PlotTypeOption(..), ColorOpt(..), plotV, oPlotV)
import HasKAL.PlotUtils.HROOT.PlotGraph3D (LogOption(..), PlotTypeOption3D(..), spectrogramM, histgram2dM)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon (rayleighMonV)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon (FitMethod(..), studentRayleighMonV)
import HasKAL.MonitorUtils.SensMon.SensMon (runSensMon)
-- import HasKAL.MonitorUtils.NoiseFloorMon.NoiseFloorMon (getNoiseFloorStatusV, makeNFMparam, estimateThreshold)
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMon)

import HasKAL.ExternalUtils.KAGALI.KAGALIUtils (nha, formatNHA, butterBandPass)
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
  let gps' = fromJust $ gps params
      duration' = duration params
      channel1' = channel1 params
      monitors' = monitors params
  forM channel1' $ \ch -> do
    datMaybe <- kagraDataGet (read gps') (read duration') ch
    case datMaybe of
     Nothing -> return ("ERROR: Can't find file or channel", ch, []) -- データが無ければメッセージを返す
     _ -> do
       fs <- liftM fromJust $ (`getSamplingFrequency` ch) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch)
       let fmin' = show $ max 0.0 $ read $ fmin params -- 負の周波数なら0Hzを返す
           fmax' = show $ min (fs/2) $ read $ fmax params -- ナイキスト周波数を超えたらナイキスト周波数を返す
       let dat = fromJust datMaybe
           tvec = V.fromList $ take (V.length dat) [0,1/fs..]
           nfft = case (truncate fs * 2) < (V.length dat) of -- データが1秒以下の時のケア
                   True -> truncate fs
                   False -> V.length dat `div` 20 -- 1秒以下のデータは20分割(20は適当)
           snf = gwpsdV dat nfft fs
           hfs = gwspectrogramV 0 nfft fs dat
           fRange = (read fmin', read fmax') -- plotツール用レンジ
       filesL <- forM monitors' $ \mon -> do
         case mon of
          "NHA" -> do -- 1モニタ複数プロット
            let pngfile1 = pngDir++ch++"_"++gps'++"_"++mon++"-A_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
                pngfile2 = pngDir++ch++"_"++gps'++"_"++mon++"-F_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
            pngExist <- doesFileExist pngfile1
            case pngExist of
             True -> return [pngfile1, pngfile2] -- 既にPNGがあれば何もしない
             False -> do
               let eiDat = butterBandPass dat fs (read fmin') (fmaxfs $ read fmax') 6 -- とりあえず6次固定
                     where fmaxfs 0 = fs/2
                           fmaxfs f = f
               case eiDat of
                Left msg -> return ["ERROR: "++msg] -- フィルタエラーならメッセージを返す
                Right dat' -> do
                  let output = formatNHA $ nha dat' fs 4 1024 256 0 (V.length dat')
                  oPlotV Linear Point 1 [RED, BLUE, PINK, GREEN, BLACK, CYAN, YELLOW] ("time [s] since GPS="++gps', "Amplitude") 0.05
                    ("NHA: "++ch) pngfile1 ((0,0),(0,0)) $ (output!!0)
                  oPlotV Linear Point 1 [RED, BLUE, PINK, GREEN, BLACK, CYAN, YELLOW] ("time [s] since GPS="++gps', "frequency [Hz]") 0.05 
                    ("NHA: "++ch) pngfile2 ((0,0),fRange) $ (output!!1)
                  return [pngfile1, pngfile2]
            -- return [""]
          _ -> do -- 1モニタ1プロット
            let pngfile = pngDir++ch++"_"++gps'++"_"++mon++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
            pngExist <- doesFileExist pngfile
            case (pngExist, mon) of
             (True, _) -> return () -- 既にPNGがあれば何もしない
             {-- Time Series Plot --}
             (_, "TS") -> do
               plotV Linear Line 1 BLUE ("time [s] since GPS="++gps', "") 0.05 ("Time Series: "++ch) pngfile ((0,0),(0,0)) (tvec, dat)
             {-- Spectrum Plot --}
             (_, "PSD") -> do
               plotV LogXY Line 1 BLUE ("frequency [Hz] (GPS="++gps'++")", "/rHz") 0.05 ("Spectrum: "++ch) pngfile (fRange,(0,0)) snf
             {-- Spectrogram Plot --}
             (_, "SPE") -> do
               histgram2dM LogZ COLZ ("time [s] since GPS="++gps', "frequency [Hz]", "/rHz") ("Spectrogram: "++ch) pngfile ((0,0),fRange) hfs
             {-- Rayleigh Monitor --}
             (_, "RM") -> do
               let ndf = case (truncate fs `div` nfft) > 16 of -- 周波数分解能
                          True -> 1 -- データ長T<1/16s なら df=1/T
                          False -> truncate $ 16 * (fromIntegral nfft) / fs -- 16Hz に固定
               let qv = rayleighMonV [0.5, 0.9, 0.95, 0.99] fs nfft ndf snf hfs
               oPlotV Linear LinePoint 1 [RED, RED, BLUE, BLUE, PINK, PINK, GREEN, GREEN]
                  ("frequency [Hz] (GPS="++gps'++")", "Normalized Noise Lv.") 0.05 ("RayleighMon: "++ch)
                 pngfile (fRange, (0,6)) (concat $ map (\(x,y)->[x,y]) qv) -- レンジ(0,6)は経験的に決めた
             {-- Student-Rayleigh Monitor --}
             (_, "SRM") -> do
               let ndf = case (truncate fs `div` nfft) > 16 of -- 周波数分解能
                          True -> 1 -- データ長T<1/16s なら df=1/T
                          False -> truncate $ 16 * (fromIntegral nfft) / fs -- 16Hz に固定
                   size = V.length dat `div` nfft - 1 -- 0から数えているので1引く(iKAGRA後直す)
                   nus = studentRayleighMonV (QUANT 0.95) fs nfft size size ndf snf hfs
               plotV Linear LinePoint 1 BLUE ("frequency [Hz] (GPS="++gps'++")", "nu") 0.05 ("StudentRayleighMon: "++ch) pngfile
                 (fRange,(0,0)) (getSpectrum 0 nus)
             {-- Rayleigh Monitor --}
             (_, "RMS") -> do -- パラメータは適当
               let rms = rmsMon (truncate $ (*0.5) $ read duration') fs dat [(read fmin', fmaxfs $ read fmax')]
                     where fmaxfs 0 = fs/2
                           fmaxfs x = min x (fs/2)
               oPlotV Linear LinePoint 1 [] ("time [s] since GPS="++gps', "RMS") 0.05 ("RMSMon: "++ch) pngfile ((0,0),(0,0)) rms
             {-- Sensitivity Monitor --}
             (_, "Sens") -> do
               let (sens, _) = runSensMon dat fs (truncate fs)
               histgram2dM LogXYZ COLZ ("frequency [Hz] (GPS="++gps'++")" ,"/rHz","yield") ("Spectrogram: "++ch) pngfile ((0,0),fRange) sens
             {-- Glitch Monitor --}
             (_, "Glitch") -> do
               return ()
             {-- Line Finder --}
             (_, "LineFind") -> do
               return ()
--            {-- Noise Floor Monitor --}
--            (_, "NFM") -> do -- パラメータは適当
--              let (rmSize, fltSize) = case (V.length dat) < 12800 of
--                                       True -> ((V.length dat) `div` 50, (V.length dat) `div` 5000 * 100)
--                                       False -> (128, 100)
--                  fs' = fs/2
--                  fmaxfs 0 = fs'/2
--                  fmaxfs x = min x (fs'/2)
--              param <- makeNFMparam fs' (fltSize) (rmSize*50) 0 rmSize (read fmin') (fmaxfs $ read fmax') 6 6
--               nfm <- getNoiseFloorStatusV dat fs (0,0) param
--              let nfm' = formatNFM nfm
--                    where formatNFM xs = (V.fromList [0, (fromIntegral $ 1*rmSize)/fs'..(fromIntegral $ (length xs - 1)*rmSize)/fs']
--                                         , V.fromList $ map (\(_,_,z) -> z) xs)
--               plotV Linear LinePoint 1 BLUE ("time [s] since GPS="++gps', "Amplitude") 0.05 ("NoiseFloorMon: "++ch) pngfile
--                ((0,0),(0,0)) $ nfm'
--            return ()
            return [pngfile]
       return (show fs, ch, concat filesL)

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          (dateForm params),
          channelForm params [Multi],
          "<a href=\"generateChannelList.cgi\" target=\"input\"> make channel list </a>",
          paramForm [NHA],
          monitorForm Multi [(False, TS, "Time Series"),
                             (False, PSD, "Spectrum"),
                             (False, SPE, "Spectrogram"),
                             (False, RM, "RayleighMon"),
                             (False, SRM, "StudentRayleighMon"),
                             (False, RMS, "RMSMon"),
                             (False, Sens, "<s>SensMon</s>"),
                             (False, Glitch, "<s>GlitchMon</s>"),
                             (False, LineFind, "<s>LineFinder</s>"),
                             (False, NHA, "<s>LineTracking</s>"){--,
                              (False, NFM, "<s>NoiseFloorMon</s>") --}
                             ],
          "<div><input type=\"submit\" value=\"plot view\" /></div>",
          "</form>"]

resultPage :: ParamCGI -> [(Message, String, [String])] -> String
resultPage params filenames = resultFrame params (genePngTable filenames)

{--
Channel名
  K1:PEM-EX_MAG_X_FLOOR_GpsTime_MonName_Params.png
--}
