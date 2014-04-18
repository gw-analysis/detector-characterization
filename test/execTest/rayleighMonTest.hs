{-******************************************
  *     File Name: rayleighMonTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/04/17 21:26:10
  *******************************************-}

import qualified System.IO as SIO
import qualified Foreign.C.Types as FCT

import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.MonitorUtils.RayleighMon as HMR

main :: IO ()
main = do
  -- 信号読み込み(frame file)
  -- frData <- HFF.readFrame "Channel_Name" "../sample-data/test-1066392016-300.gwf" :: IO(HFF.FrDataType [FCT.CDouble])
  -- let gwDataT = map realToFrac (HFF.eval frData) :: [Double]
  
  -- 信号読み込み(text file)
  iFile <- SIO.openFile "../sample-data/gausRand.txt" SIO.ReadMode
  dataStr <- SIO.hGetContents iFile
  let gwDataT = s2dl dataStr

  -- monitor実行
  let resultStr = dll2s $ HMR.rayleighMon 300 100 1000.0 gwDataT :: String

  -- 結果出力
  oFile <- SIO.openFile "./rayleighMonTestResult.txt" SIO.WriteMode
  SIO.hPutStrLn oFile resultStr
  SIO.hClose oFile

-- [[1.0, 2.0 ..], [3.0, 4.0 ..]..] -> "1.0 2.0..\n3.0 4.0..\n.."  
dll2s :: [[Double]] -> String
dll2s xss = unlines [unwords (map show xs) | xs <- xss]

-- "1.0 2.0..\n3.0 4.0..\n.." -> [1.0, 2.0 .. 3.0, 4.0 ..]
s2dl :: String -> [Double]
s2dl xs = map read (words xs)                                                                                                                          
