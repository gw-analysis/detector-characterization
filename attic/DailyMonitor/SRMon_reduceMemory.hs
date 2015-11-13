import Control.Monad (forM)
import Control.DeepSeq (deepseq)
import Data.List (foldl1')
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector, fromList, dim)
import Data.Packed.Matrix (fromBlocks, cols, fromLists)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV, gwspectrogramV)
import HasKAL.SpectrumUtils.Function 
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph3D


main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: dailySRMon yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 3600*24 -- seconds
      -- for SRMon
      fftLength = 1    -- seconds
      srmLength = 3600 -- seconds
      timeShift = 1800 -- seconds
      tShiftMod = mod timeShift srmLength
      freqResol = 16   -- Hz
      quantile  = 0.99 -- 0 < quantile < 1
      -- for data base
      dbchunck = srmLength
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_SRMon.png"
      title = "StudentRayleighMon: " ++ ch
      xlabel = "Date: "++year++"/"++month

  {-- 1st data read --}
  mbFiles1 <- kagraDataFind (fromIntegral gps) (fromIntegral srmLength) ch
  let file1 = case mbFiles1 of
               Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
               _ -> head $ fromJust mbFiles1
  mbFs <- getSamplingFrequency file1 ch
  let fs = case mbFs of
            Just a -> a
            Nothing -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day

  mbDat1 <- kagraDataGet gps dbchunck ch
  let dat1 = case mbDat1 of
              Just a -> a
              Nothing -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day


  {-- 1st main --}
  let snf1 = gwOnesidedPSDV (subVector 0 (truncate $ fftLength * fs * 1024) dat1) (truncate $ fftLength * fs) fs
  let hf1 = gwspectrogramV 0 (truncate $ fftLength * fs) fs $ dat1
  nu1 <- do
    let res = studentRayleighMonV (QUANT quantile) fs (truncate $ fftLength * fs) srmLength timeShift (truncate $ freqResol/fftLength) snf1 hf1
    deepseq res $ return $
      (\(_,fv,nu) -> (fromList $ map (\n -> fromIntegral (n * timeShift)) [0..cols nu - 1], fv, nu)) res


  {-- loop of remaining data --}
  nus <- forM [gps-tShiftMod+dbchunck, gps-tShiftMod+2*dbchunck..gps+duration-tShiftMod-dbchunck] $ \tau -> do
    {---- data read ----}
    mbDat <- kagraDataGet tau (dbchunck+tShiftMod) ch
    case mbDat of
     Nothing -> return (fromList [], fromList [0, 1/fftLength..fs/2], fromLists [[]])
     Just dat -> do

       {------ main ------}
       let snf = gwOnesidedPSDV (subVector 0 (truncate $ fftLength * fs * 1024) dat) (truncate $ fftLength * fs) fs
       let hf  = gwspectrogramV 0 (truncate $ fftLength * fs) fs $ dat
           nus = studentRayleighMonV (QUANT quantile) fs (truncate $ fftLength * fs) srmLength timeShift (truncate $ freqResol/fftLength) snf1 hf
       deepseq nus $ return $ (\(_,fv,nu) -> (fromList $ map (\n -> fromIntegral (tau +  n * timeShift)) [0..cols nu - 1], fv, nu)) nus


  {-- plot --}
  let result = foldl1' (mergeSpectrogram (fromIntegral timeShift)) $ nu1:nus
  histgram2dDateM Linear COLZ (xlabel, "frequency [Hz]", "nu") title oFile ((0,0),(0,0)) gps result

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

mergeSpectrogram :: Double -> Spectrogram -> Spectrogram -> Spectrogram
mergeSpectrogram dt (t1, f1, x1) (t2, f2, x2) = (t3, f1, x3)
  where t3 = fromList [0, dt..(len-1)*dt]
        len = fromIntegral $ (dim t1) + (dim t2)
        x3 = fromBlocks [[x1, x2]]
