{-******************************************
  *     File Name: cuiSrm.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/07/15 16:51:19
  *******************************************-}

import qualified Control.Monad as CM
import qualified System.Environment as SE
import qualified System.IO.Unsafe as SIOU -- データ読み出し

import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.FrameUtils.FrameUtils as HFF -- データ読み出し
import qualified HasKAL.FrameUtils.PickUpFileName as HFP -- データ読み出し
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS -- データ読み出し
import qualified HasKAL.Misc.StrictMapping as HMS
import qualified HasKAL.Misc.Flip3param as HMF 
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighMon as SRM
import qualified HasKAL.PlotUtils.PlotUtils as HPP
import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS

data DType = Time | Freq | LigoS6 deriving (Eq, Show)
data Plot = X11 | PNG deriving (Eq)

plot :: Plot
plot = X11

{--  実データでは必要ないもの  --}
nuTrue :: Double
nuTrue = 15 -- 非ガウスさパラメータnu

seed :: Integer
seed = -1 -- 乱数の初期シード(負の値でUNIX Timeが入る)


{--  テストコード  --}
main = do
  {--  GUIから与えるパラメータ  --}
  (channel, fs, flag) <- CM.liftM paramCheck $ SE.getArgs
  let dT = 1.0                  -- SFTストライド[sec]
      dF = 16.0                 -- 周波数解像度[Hz]
      sT = 128 :: Int           -- チャンク幅[sec]
      aveNum = 1024 :: Int      -- Sn(f)の平均回数
      overlap = 1.0 - 1.0/8.0   -- 時間シフト幅(0 <= x < 1)
      method = SRM.LSM          -- Fitting方法
      gps = 959201088           -- GPS時刻
      cache = HGGS.haskalOpt ++ "/cachefiles/cachefile_LD.lst"

  {--  与えられたパラメータから自動的に決まるもの  --}
  let nT = truncate $ fs * dT
      df = 1.0 / dT
      dTau = fromIntegral sT * (1.0 - overlap)
      nF = truncate $ dF * dT
      chunck = truncate fs * sT
      nC = truncate $ (fromIntegral sT) * fs / (fromIntegral nT)
      num = 3 :: Int

  {--  データ生成  --}
  rng <- RNG.newRngWithSeed seed
  -- 時系列データを生成
  (snfs, hfs) <- case flag of
    Time -> do
      snt <- HMS.forM' [0..nT*aveNum] $ \idx -> RND.gslRanTdist rng nuTrue
      let snfs = map snd $ (HMF.flip231 HSS.gwpsd nT fs) snt
      hts <- HMS.forM' [0..chunck*num] $ \idx -> RND.gslRanTdist rng nuTrue
      let hfs = map (map sqrt) $ map (map snd) $ map (HMF.flip231 HSS.gwpsd nT fs) $ SRM.dataSplit nT 0 hts
      return (snfs, hfs)
  -- 直接スペクトルを生成
    Freq -> do
      snfs <- CM.forM [0..nT`div`2] $ \jdx -> do -- 全周波数ビンについて計算
        snf <- CM.forM [1..aveNum] $ \idx -> do -- １つのビンの(aveNum回)平均スペクトル
          nr <- RND.gslRanTdist rng nuTrue
          ni <- RND.gslRanTdist rng nuTrue
          return $ (nr*nr + ni*ni)
        return $ (/ (fromIntegral aveNum) ) $ (sum snf) -- パワー平均
      hfs <- CM.forM [1..nC*num] $ \jdx -> do -- スペクトルを複数セット
        hf <- CM.forM [0..nT`div`2] $ \idx -> do -- 各周波数ビンについてデータ生成
          hr <- RND.gslRanTdist rng nuTrue
          hi <- RND.gslRanTdist rng nuTrue
          return $ sqrt $ (hr*hr + hi*hi)
        return hf
      return (snfs, hfs)
  -- LIGOデータ読み込み
    LigoS6 -> do
      let snfs = getAvePsdFromGPS nT fs aveNum gps channel cache
      let hts = getDataFromGPS gps (fromIntegral $ sT * num) channel cache
      let hfs = map (map sqrt) $ map (map snd) $ map (HMF.flip231 HSS.gwpsd nT fs) $ SRM.dataSplit nT 0 hts
      return (snfs, hfs)

  {--  nu探索  --}
  let nu = SRM.studentRayleighMon' method nC overlap nF snfs hfs
  case plot of
    X11 -> HPP.scatter_plot_3d "SRMon" channel 2.0 (720,480) $ timeFreqData [0, dTau..] [0, dF..fs/2] nu
    PNG -> do
      HPP.scatter_plot_3d_png "SRMon" channel 2.0 ("./fig/"++channel++".png") $ timeFreqData [0, dTau..] [0, dF..fs/2] nu
      return ()


{--  データ整形用  --}
timeFreqData :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
timeFreqData [] _ _ = []
timeFreqData _ [] _ = []
timeFreqData _ _ [] = []
timeFreqData (x:xs) ys (zs:zss) = zip3 [x,x..] ys zs ++ timeFreqData xs ys zss


{--  データ読み出し用  --}
getAvePsdFromGPS :: Int -> Double -> Int -> Integer -> String -> String -> [Double]
getAvePsdFromGPS numT fs aveNum gpsD channel cache = map snd.(HMF.flip231 HSS.gwpsd numT fs).(take $ aveNum*numT).concat $ datW
  where datW = SIOU.unsafePerformIO $ mapM (readFrame' channel) $ HFP.pickUpFileNameinFile gpsW (gpsD-1) cache
        gpsW = (-) gpsD $ ceiling $ (fromIntegral $ aveNum*numT) / fs

getDataFromGPS :: Integer -> Integer -> String -> String -> [Double]
getDataFromGPS gpsD obsD channel cache = concat $ SIOU.unsafePerformIO $ mapM (readFrame' channel) filelist
  where filelist = HFP.pickUpFileNameinFile gpsD (gpsD+obsD-1) cache

readFrame' :: String -> String -> IO [Double]
readFrame' = (CM.liftM ((map realToFrac).HFF.eval).).HFF.readFrame

paramCheck :: [String] -> (String, Double, DType)
paramCheck [] = ("simF", 1024, Freq)
paramCheck (x:[]) = ("simT", 1024, Time)
paramCheck (x:y:_) = (x, read y, LigoS6)
