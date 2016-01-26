
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Vector.Storable (Vector, fromList, toList)
import qualified Data.Vector.Storable as V (slice)
-- import Data.List (transpose)
-- import qualified Data.Vector.Storable as V (modify, head, length)
-- import Data.Packed.Matrix (fromColumns, toRows, Matrix, mapMatrix, cols, rows)
-- import qualified Data.Vector.Algorithms.Heap as H (sort, select)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV, gwspectrogramV)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph
-- import HasKAL.SpectrumUtils.Signature
-- import HasKAL.Misc.StrictMapping (forM')

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: RMon yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for SRMon
      fftLength = 1    -- seconds
      freqResol = 16   -- Hz
      quantiles  = [0.50, 0.95, 0.99] -- 0 < quantile < 1
  -- let partSec = 864 -- 1日を100分割
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_RMon.png"
      title = "RayleighMon(RED=0.5, BLUE=0.95, PINK=0.99): " ++ ch
      xlabel = "frequency [Hz] at "++year++"/"++month++"/"++day

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let file = case mbFiles of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust mbFiles
  mbDat <- kagraDataGet gps duration ch
  mbFs <- getSamplingFrequency file ch
  let (dat, fs) = case (mbDat, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day

  {-- main --}
  let snf = gwOnesidedPSDV (V.slice 0 (truncate $ fftLength * fs * 1024) dat) (truncate $ fftLength * fs) fs
      hf  = gwspectrogramV 0 (truncate $ fftLength * fs) fs dat
      result = rayleighMonV quantiles fs (truncate $ fftLength * fs) (truncate $ freqResol/fftLength) snf hf
  oPlotV Linear [Line, LinePoint, Line, LinePoint, Line, LinePoint] 1 [RED, RED, BLUE, BLUE, PINK, PINK]
    (xlabel, "normalized noise Lv.") 0.05 title oFile ((0,0),(0,10)) $ concat $ map (\(x,y) -> [x,y]) result

  -- result <- forM' [0,1..(V.length dat)`div`partSec`div`(truncate fs)-1] $ \idx -> do 
  --   let dat' = V.slice (idx*partSec*(truncate fs)) (partSec*(truncate fs)) dat :: Vector Double
  --       hf   = gwspectrogramV 0 (truncate $ fftLength * fs) fs dat'
  --       result = rayleighMonV quantiles fs (truncate $ fftLength * fs) (truncate $ freqResol/fftLength) snf hf :: [(Spectrum, Spectrum)]
  --   return $ map fst result :: IO [Spectrum]
  -- unsafePlot3d ((1,1280),(3,960)) Linear COLZ (replicate len (xlabel, "normalized noise Lv. [/rHz]", "yield")) 
  --   titles oFile (replicate (length quantiles) ((0,0), (0,0))) $ rayleighHist result


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
