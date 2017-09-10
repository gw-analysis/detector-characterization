 --- テストコード
 -- 注:このソースコード自体にIOは含まれていないので、ここに書いてあるテストコードはこのプログラムの一番下にあるIOプログラム群を通して出力したものになっています。ご了承下さい。
 --  distInspiralCore
 --  *Main> distInspiralCoreCulculation 1.4 1.4 8 "kagraPsd.dat" 10
 --  288.156441492326

 --  distInspiral
 --  *Main> distInspiralCulculation 1.4 1.4 "kagraPsd.dat"
 --  288.156441492326

 --  distRingdownCore
 --  *Main> distRingdownCoreCulculation 300 8 0.98 0.03 0.0 "kagraPsd.dat" 2048 10
 --  15840.358769466673

 --  distRingdown
 --  *Main> distRingdownCulculation 300 8 0.98 0.03 0.0 "kagraPsd.dat" 2048 10"
 --  15840.360852109441

 --- 出典元
 --  snrInspiral:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B3)
 --  snrRingdown:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B10)



module HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta
       (distRingdownCore
       ,distRingdown
       ,distInspiralCore
       ,distInspiral)where

import Numeric.LinearAlgebra
import Data.List
import HasKAL.Constant.MKSA
import qualified Data.Vector.Storable as V

dim :: V.Vector Double -> Int
dim = V.length


 -- 光速の定義[m/s]
c :: Double
c  = hasKAL_const_mksa_speed_of_light

 -- 万有引力定数の定義[m^3 kg^-1 s^-2]
g :: Double
g = hasKAL_const_mksa_gravitational_constant

 -- 太陽質量[kg]
msolar :: Double
msolar = hasKAL_const_mksa_solar_mass

 -- Mpc[m]
megapc :: Double
megapc = hasKAL_const_mksa_parsec * 10**(6)


 --- distInspiral:インスパイラルの距離を計算する準備をし距離を返す
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] SNR 使用する検出器の[(周波数,ノイズパワースペクトル)]データ
distInspiral :: Double ->  Double -> [(Double,Double)] -> Double
distInspiral  msol1 msol2 ifo = distInspiralCore msol1 msol2 8 ifo 10


 --- snrInspiralCore:インスパイラルの距離を計算する準備をし、周波数カットオフもパラメータに加えた上で距離を返す
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] 連星までの距離[Mpc] 使用する検出器の[(周波数,ノイズパワースペクトル)]データ 周波数cutoff下限[Hz]
distInspiralCore :: Double -> Double -> Double -> [(Double,Double)] -> Double -> Double
distInspiralCore  msol1 msol2 snr ifo flower
  | flower >= fupp = 0
  | otherwise = getsnrInspiral
  where
        fupp =  1/((6**(3/2)) *pi*(msol1 +  msol2)*(msolar)*(g/(c**3)))
        readnumwithfrequencycut = [(a,b)|(a,b)<-ifo, a>=flower, b<=fupp]
        (numfreq, numnois) = unzip readnumwithfrequencycut -- 周波数,PSDのリストを作成
        vectnumhead = fromList numfreq -- 周波数データをリストからVectorに変換
        nf = dim vectnumhead
        f1 = subVector 0 (nf - 1) vectnumhead
        f2 = subVector 1 (nf - 1) vectnumhead
        df = zipWith (-) (toList f2) (toList f1) -- 離散データ間の周波数刻み幅のリストを作成
        mapInspiralwithparam = zipWith (*) df (integratedInspiral readnumwithfrequencycut) -- zipWithで離散データの周波数、パワースペクトルのリストをintegratedwithparamに適用し、リストを返す。更にそのリスト全てに対応する周波数刻み幅の重み付けをする
        snrRingdownPow2 = sum mapInspiralwithparam -- 作成したリストを全て足し合わせる
        getsnrInspiral = distInspiralculc msol1 msol2 snr snrRingdownPow2 -- SNRを計算する


 --- integratedInspiral:インスパイラルの被積分関数を定義
 -- 引数
 -- 使用する検出器の(周波数,パワースペクトル)データの 周波数 及び パワースペクトル
integratedInspiral :: [(Double,Double)] -> [Double]
integratedInspiral psd =  map (\(x,y)-> x**(-7/3)/y) psd


 --- distInspiralculc:インスパイラルの距離を定義
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] SNR インスパイラル被積分関数の和
distInspiralculc :: Double -> Double -> Double -> Double -> Double
distInspiralculc msol1 msol2 snr snrInspiralPow2= 1.77*(cons*(allmass**(5/96))*((5*symmass/6)**(1/2))/(snrstand * pi**(2/3)))*(snrInspiralPow2**(1/2))
  where allmass =  (msol1 +  msol2)*(msolar)
        symmass =  msol1*msol2/((msol1 +  msol2)**2)
        snrstand =  snr*(megapc)
        cons =  c*((g/(c**3))**(5/6))


 --- distRingdown:リングダウンの距離を計算する準備をし、距離を返す
 -- 引数
 -- BH質量[太陽質量] SNR Kerr parameter 質量欠損比率 初期位相 使用する検出器のリストデータ 周波数cutoff上限[Hz] 周波数cutoff下限[Hz]
distRingdown :: Double -> [(Double,Double)] -> Double
distRingdown msol ifo = distRingdownCore msol 8 0.98 0.03 0.0 ifo 2048 30


 --- distRingdownCore:リングダウンの距離を計算する準備をし、周波数カットオフもパラメータに加えた上で距離を返す
 -- 引数
 -- BH質量[太陽質量] SNR Kerr parameter 質量欠損比率 初期位相 使用する検出器のリストデータ 周波数cutoff上限[Hz] 周波数cutoff下限[Hz]
distRingdownCore :: Double -> Double -> Double -> Double -> Double -> [(Double,Double)] -> Double -> Double -> Double
distRingdownCore  msol snr a epsil phi ifo fupp flower = getsnrRingdown
  where
        readnumwithfrequencycut = [(a,b)|(a,b)<-ifo, a>=flower, b<=fupp]
        (numfreq, numnois) = unzip readnumwithfrequencycut -- 周波数,PSDのリストを作成
        vectnumhead = fromList numfreq -- 周波数データをリストからVectorに変換
        nf = dim vectnumhead
        f1 = subVector 0 (nf - 1) vectnumhead
        f2 = subVector 1 (nf - 1) vectnumhead
        df = zipWith (-) (toList f2) (toList f1) -- 離散データ間の周波数刻み幅のリストを作成
        mapRingdownwithparam = zipWith (*) df (integratedRingdown msol a phi readnumwithfrequencycut) -- zipWithで離散データの周波数、パワースペクトルのリストをintegratedwithparamに適用し、リストを返す。更にそのリスト全てに対応する周波数刻み幅の重み付けをする
        snrRingdownPow2 = sum mapRingdownwithparam -- 作成したリストを全て足し合わせる
        getsnrRingdown = distRingdownculc msol snr a epsil snrRingdownPow2 -- 距離を計算する


 --- integratedRingdown:リングダウンの距離の被積分関数を定義
 -- 引数
 -- BH質量[太陽質量] Kerr parameter 使用する検出器 検出器のノイズスペクトル
integratedRingdown :: Double -> Double -> Double -> [(Double,Double)] -> [Double]
integratedRingdown msol a phi psd =
  map (\(fin,noiseSpec)->((cos(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) + 1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2)
   + (sin(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) -  1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2)) /noiseSpec) psd
  where q = 2*(1 - a)**( - 9/20)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)
        m = (g*msol*(msolar)/(c**(3)))


 --- distRingdownculc:リングダウンの距離を定義
 -- 引数
 -- BH質量[太陽質量] SNR Kerr parameter 質量欠損比率 初期位相 使用する検出器
distRingdownculc :: Double -> Double -> Double -> Double -> Double -> Double
distRingdownculc msol snr a epsil snrRingdownPow2 = ((((10*epsil*m*fqnr)/(q*pi*(1 + 7/(24*(q**2)))))**(1/2))*(snrRingdownPow2)**(1/2))/(pi*q*(snrstand/c))
  where q = 2*(1 - a)**( - 9/20)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)
        m = (g*msol*(msolar)/(c**(3)))
        snrstand = snr*(megapc)


-- import System.Environment
-- import System.IO
-- import Data.List
-- import Control.Monad

-- distInspiralCulculation :: Double -> Double -> FilePath -> IO()
-- distInspiralCulculation msol1 msol2 ifo = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distInspiral msol1 msol2 ifolist2

-- distInspiralCoreCulculation :: Double -> Double -> Double -> FilePath -> Double -> IO()
-- distInspiralCoreCulculation msol1 msol2 snr ifo flower = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distInspiral msol1 msol2 snr ifolist2

-- distRingdownCulculation ::  Double -> FilePath -> IO()
-- distRingdownCulculation msol ifo = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distRingdown msol ifolist2

-- distRingdownCoreCulculation :: Double -> Double -> Double -> Double -> Double -> FilePath -> Double -> Double -> IO()
-- distRingdownCoreCulculation msol snr a epsil phi ifo fupp flower = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distRingdownCore msol snr a epsil phi ifolist2 fupp flower


-- tuplify2 ::  [a] ->  (a,a)
-- tuplify2 [x,y] = (x,y)
