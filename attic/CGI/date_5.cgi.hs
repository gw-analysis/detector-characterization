import Debug.Trace (trace)

import Network.CGI
import Data.Maybe (fromJust)
import Control.Monad (forM, liftM)
import System.Directory (doesFileExist)

import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Function (fromSpectrum, toSpectrum)
import HasKAL.PlotUtils.HROOT.PlotGraph (LogOption(..), PlotTypeOption(..), ColorOpt(..), plotV, oPlotV)
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta (distInspiral, distRingdown)
import HasKAL.MonitorUtils.RangeMon.IMBH (distImbh)
import HasKAL.WebUtils.CGI.Function
import SampleChannel

-- 主干渉信号が無いので、感度曲線で計算
import qualified Data.Vector.Storable as V (fromList, map)
import HasKAL.SpectrumUtils.DetectorSensitivity (ifonoisepsd, Detector(..))

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
  case (gps params, monitors params) of
   (Nothing, _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (Just "", _) -> return $ inputForm $ updateMsg "" $ updateGps nowGps params
   (_, [])      -> return $ "<html><body><h1>unselected monitor</h1></body></html>"
   (Just x, _)  -> do
     fnames <- process params
     return $ resultPage params fnames
   -- (_, []) ->  do
   --   let params' = defaultMon ["INSP"] params
   --   fnames <- process params'
   --   return $ resultPage params' fnames
   -- (Just x, _)  -> do
   --   fnames <- process params
   --   return $ resultPage params fnames

process :: ParamCGI -> IO [(Message, String, [String])]
process params = do
  let gps' = fromJust $ gps params
      duration' = duration params
      fmin' = fmin params
      fmax' = fmax params
      monitors' = monitors params
      ch1 = "K1:GW-channel"
  datMaybe <- return (Just "a") --- kagraDataGet (read gps') (read duration') ch1
  case datMaybe of
   Nothing -> return [("Can't find file or GW-channel", "", [])]
   _ -> do
     --- fs <- liftM fromJust $ (`getSamplingFrequency` ch1) =<< liftM (head.fromJust) (kagraDataFind (read gps') (read duration') ch1)
     --- let dat = fromJust datMaybe
     ---     fromSpectrum $ gwpsdV dat (truncate fs) fs
     let snf = fromSpectrum $ (\x -> (x, V.map (*2) $ ifonoisepsd KAGRA x)) $ V.fromList [1.0,2..2048]
         fRange = (read fmin', read fmax') :: (Double, Double)
     files <- forM monitors' $ \mon -> do
       let pngfile = pngDir++ch1++"_"++gps'++"_"++mon++"_"++duration'++"_fl"++fmin'++"_fh"++fmax'++".png"
       pngExist <- doesFileExist pngfile
       case pngExist of
        True -> return () -- 既にPNGがあれば何もしない
        False -> do
          case mon of
           "INSP" -> do
             let mass = [1.0, 1.2..9.8]++[10.0, 12.0..98.0]++[100, 120..200]
                 dist = toSpectrum $ zip mass $ map (\m -> distInspiral m m snf) mass
             plotV LogXY Line 2 BLUE ("M_sol(m1=m2)", "Mpc") 0.05 ("Inspiral Range(SNR=10): GPS="++gps') pngfile ((0,0),(0,0)) dist
           "RD" -> do
             let mass = [10.0, 15.0..95.0]++[100.0, 150.0..950.0]++[1000, 1500..9500]
                 dist = toSpectrum $ zip mass $ map (\m -> distRingdown m snf) mass
             plotV LogXY Line 2 BLUE ("M_sol", "Mpc") 0.05 ("RingDown Range(SNR=10): GPS="++gps') pngfile ((0,0),(0,0)) dist
           "IMBH" -> do
             let mass = [10.0, 12.0..98.0]++[100, 120..1000]
                 dist = toSpectrum $ zip mass $ map (\m -> distImbh m m snf) mass
             plotV LogXY Line 2 BLUE ("M_sol(m1=m2)", "Mpc") 0.05 ("Insp-Merg-RingD Range(SNR=10): GPS="++gps') pngfile ((0,0),(0,0)) dist
           "Stoch" -> do
             return ()
       return pngfile
     return [("", ch1, files)] -- [(show fs1, ch1, files)] -- 後で変える

inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\"plotframe\">",
          (dateForm params),
          paramForm [],
          monitorForm Multi [(True, INSP, "Inspiral")
                            ,(False, RD, "RingDown")
                            ,(False, IMBH, "Inspiral-Merger-RingDown")
                            ,(False, Stoch, "<s>StochMon</s>")
                            ],
          "<br><center>",
          "<div style=\"padding:15px 15px;",
          "background-color:coral;width:80px;border-radius:20px;\">",
          "<input type=\"submit\" value=\"plot view\" style=\"font-size:16px\"></div>",
          "</center>",
          "</form>"]

resultPage :: ParamCGI -> [(Message, String, [String])] -> String
resultPage params filenames = resultFrame params (genePngTable filenames)
