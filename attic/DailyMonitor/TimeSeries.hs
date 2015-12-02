
import System.Environment (getArgs)
import Data.Packed.Vector (Vector, subVector, dim, fromList)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getUnitY)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Resampling (downsampleSV)

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: TimeSeries yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      dsfs = 2048
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_TimeSeries.png"
      title = "TimeSeries: " ++ ch
      xlabel = "Date: "++year++"/"++month

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let file = case mbFiles of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              Just x -> head x
  mbDat <- kagraDataGet gps duration ch
  mbFs <- getSamplingFrequency file ch
  let (dat, fs) = case (mbDat, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day
  mbUnit <- getUnitY file ch
  let unit = case mbUnit of
              Just x  -> "["++x++"]"
              Nothing -> ""

  {-- main --}
  let (minfs, dat') = case (fs > dsfs) of
                       True -> (dsfs, dropBothSide 8 $ downsampleSV fs dsfs dat)
                       False -> (fs, dat)
  let tvec = fromList [0, 1/dsfs..(fromIntegral $ dim dat' - 1)/minfs]
  print $ dim dat'
  plotDateV Linear Line 1 RED (xlabel, "amplitude "++unit) 0.05 title oFile ((0,0),(0,0)) gps (tvec, dat')

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

dropBothSide :: Int -> Vector Double -> Vector Double
dropBothSide n xv 
  | len <= 2*n = fromList []
  | otherwise  = subVector n (len-2*n) xv
  where len = dim xv
