 --- テストコード
 -- 注:このソースコード自体にIOは含まれていないので、ここに書いてあるテストコードはこのプログラムの一番下にあるIOプログラム群を通して出力したものになっています。ご了承下さい。
 --  snrInspiralCore
 --  *Main> snrInspiralCoreCulculation 1.4 1.4 280 "kagraPsd.dat" 10
 --  8.23304118549503

 --  snrInspiral
 --  *Main> snrInspiralCulculation 1.4 1.4 280 "kagraPsd.dat"
 --  8.23304118549503

 --  snrRingdownCore
 --  *Main> snrRingdownCoreCulculation 300 15840 0.98 0.03 0.0 "kagraPsd.dat" 2048 10
 --  8.00018224854012

 --  distRingdown
 --  *Main> snrRingdownCulculation 300 15840 "kagraPsd.dat"
 --  8.00018224854012

 --- 出典元
 --  snrInspiral:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B3)
 --  snrRingdown:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B10)



module HasKAL.MonitorUtils.RangeMon.InspiralRingdownSnrQuanta
       (snrRingdownCore
       ,snrRingdown
       ,snrInspiralCore
       ,snrInspiral)where

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


 --- snrInspiral:インスパイラルのSNRを計算する準備をしSNRを返す
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] 連星までの距離[Mpc] 使用する検出器の(周波数,ノイズパワースペクトル)データ
snrInspiral :: Double -> Double -> Double -> [(Double,Double)] -> Double
snrInspiral  msol1 msol2 dmpc ifo = snrInspiralCore msol1 msol2 dmpc ifo 10



 --- snrInspiralCore:インスパイラルのSNRを計算する準備をし、周波数カットオフもパラメータに加えた上でSNRを返す
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] 連星までの距離[Mpc] 使用する検出器の(周波数,ノイズパワースペクトル)データ 周波数cutoff下限[Hz]
snrInspiralCore :: Double -> Double -> Double -> [(Double,Double)] -> Double -> Double
snrInspiralCore  msol1 msol2 dmpc ifo flower
  | flower >= fupp = 0
  | otherwise = getsnrInspiral
  where ifonontupl = map invtuplify2 ifo
        fupp =  1/((6**(3/2)) *pi*(msol1 +  msol2)*(msolar)*(g/(c**3)))
        readnumwithfrequencycut = updowncut ifonontupl flower fupp -- データの周波数を必要な分だけ取り出す(詳しくはupdowncut関数を参照)
        numfreq = map head readnumwithfrequencycut -- 周波数データのみのリストを作成
        numnois = map last readnumwithfrequencycut -- パワースペクトルのみのリストを作成
        vectnumhead = fromList numfreq -- 周波数データをリストからVectorに変換
        nf = dim vectnumhead
        f1 = subVector 0 (nf - 1) vectnumhead
        f2 = subVector 1 (nf - 1) vectnumhead
        df = zipWith (-) (toList f2) (toList f1) -- 離散データ間の周波数刻み幅のリストを作成
        mapInspiralwithparam = zipWith (*) df (init (zipWith integratedInspiral numfreq numnois)) -- zipWithで離散データの周波数、パワースペクトルのリストをintegratedwithparamに適用し、リストを返す。更にそのリスト全てに対応する周波数刻み幅の重み付けをする
        snrRingdownPow2 = foldr (+) 0 mapInspiralwithparam -- 作成したリストを全て足し合わせる
        getsnrInspiral = snrInspiralculc msol1 msol2 dmpc snrRingdownPow2 -- SNRを計算する


 --- integratedInspiral:インスパイラルのSNRの被積分関数を定義
 -- 引数
 -- 使用する検出器の(周波数,パワースペクトル)データ 周波数
integratedInspiral :: Double -> Double -> Double
integratedInspiral fin noiseSpec =  fin**( - 7/3)/noiseSpec


 --- snrInspiralculc:インスパイラルのSNRを定義
 -- 引数
 -- BH質量[太陽質量] BHまでの距離[Mpc] Kerr parameter 質量欠損比率 初期位相 使用する検出器
snrInspiralculc :: Double -> Double -> Double -> Double -> Double
snrInspiralculc msol1 msol2 dmpc snrInspiralPow2= (cons*(allmass**(5/6))*((5*symmass/6)**(1/2))/(d * pi**(2/3)))*(snrInspiralPow2**(1/2))
  where allmass =  (msol1 +  msol2)*(msolar)
        symmass =  msol1*msol2/((msol1 +  msol2)**2)
        d =  dmpc*(megapc)
        cons =  c*((g/(c**3))**(5/6))


 --- snrRingdown:リングダウンのSNRを計算する準備をし、SNRを返す
 -- 引数
 -- BH質量[太陽質量] BHまでの距離[Mpc] Kerr parameter 質量欠損比率 初期位相 使用する検出器の[(周波数,パワースペクトル)]データ
snrRingdown :: Double -> Double -> [(Double,Double)] -> Double
snrRingdown msol dmpc ifo = snrRingdownCore msol dmpc 0.98 0.03 0.0 ifo 2048 10


 --- snrRingdownCore:リングダウンのSNRを計算する準備をし、周波数カットオフもパラメータに加えた上でSNRを返す
 -- 引数
 -- BH質量[太陽質量] BHまでの距離[Mpc] Kerr parameter 質量欠損比率 初期位相 使用する検出器の[(周波数,パワースペクトル)]データ 周波数cutoff上限[Hz] 周波数cutoff下限[Hz]
snrRingdownCore :: Double -> Double -> Double -> Double -> Double -> [(Double,Double)] -> Double -> Double -> Double
snrRingdownCore  msol dmpc a epsil phi ifo fupp flower = getsnrRingdown
  where ifonontupl = map invtuplify2 ifo
        readnumwithfrequencycut = updowncut ifonontupl flower fupp -- データの周波数を必要な分だけ取り出す(詳しくはupdowncut関数を参照)
        numfreq = map head readnumwithfrequencycut -- 周波数データのみのリストを作成
        numnois = map last readnumwithfrequencycut -- パワースペクトルのみのリストを作成
        vectnumhead = fromList numfreq -- 周波数データをリストからVectorに変換
        nf = dim vectnumhead
        f1 = subVector 0 (nf - 1) vectnumhead
        f2 = subVector 1 (nf - 1) vectnumhead
        df = zipWith (-) (toList f2) (toList f1) -- 離散データ間の周波数刻み幅のリストを作成
        integratedRingdownwithparam fin noiseSpec= integratedRingdown msol a phi fin noiseSpec -- 周波数、パワースペクトル以外の変数を指定したintegratedRingdownを返す
        mapRingdownwithparam = zipWith (*) df (init (zipWith integratedRingdownwithparam numfreq numnois)) -- zipWithで離散データの周波数、パワースペクトルのリストをintegratedwithparamに適用し、リストを返す。更にそのリスト全てに対応する周波数刻み幅の重み付けをする
        snrRingdownPow2 = foldr (+) 0 mapRingdownwithparam -- 作成したリストを全て足し合わせる
        getsnrRingdown = snrRingdownculc msol dmpc a epsil snrRingdownPow2 -- SNRを計算する


 --- integratedRingdown:リングダウンのSNRの被積分関数を定義
 -- 引数
 -- BH質量[太陽質量] Kerr parameter 使用する検出器 検出器のノイズスペクトル
integratedRingdown :: Double -> Double -> Double -> Double -> Double -> Double
integratedRingdown msol a phi fin noiseSpec = ((cos(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) + 1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2) + (sin(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) -  1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2)) /noiseSpec
  where q = 2*(1 - a)**( - 9/20)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)
        m = (g*msol*(msolar)/(c**(3)))


 --- snrRingdownculc:リングダウンのSNRを定義
 -- 引数
 -- BH質量[太陽質量] BHまでの距離[Mpc] Kerr parameter 質量欠損比率 初期位相 使用する検出器
snrRingdownculc :: Double -> Double -> Double -> Double -> Double -> Double
snrRingdownculc msol dmpc a epsil snrRingdownPow2 = ((((10*epsil*m*fqnr)/(q*pi*(1 + 7/(24*(q**2)))))**(1/2))*(snrRingdownPow2)**(1/2))/(pi*q*(d/c))
  where q = 2*(1 - a)**( - 9/20)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)
        m = (g*msol*(msolar)/(c**(3)))
        d = dmpc*(megapc)


--- updowncut:指定した周波数バンドのみのデータを使用するよう、周波数の上限、下限を引数に取りその分のデータのみを取得する
 -- 引数
 -- 検出器の周波数及びノイズパワースペクトルデータのリスト 周波数カットオフ下限 周波数カットオフ上限
updowncut :: [[Double]] -> Double -> Double -> [[Double]]
updowncut xs flower fupp
  | head (head xs) < flower = updowncut (tail xs) flower fupp
  | head (last xs) > fupp = updowncut (init xs) flower fupp
  | otherwise = xs


 --- invtuplify2:2引数のタプルからリストヘ変換する
 -- 引数
 -- 2変数のタプル
invtuplify2 ::  (a,a) ->  [a]
invtuplify2 (x,y) = [x,y]


-- import System.Environment
-- import System.IO
-- import Data.List
-- import Control.Monad

-- snrInspiralCulculation :: Double -> Double -> Double -> FilePath -> IO()
-- snrInspiralCulculation msol1 msol2 dmpc ifo = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ snrInspiral msol1 msol2 dmpc ifolist2

-- snrInspiralCoreCulculation :: Double -> Double -> Double -> FilePath -> Double -> IO()
-- snrInspiralCoreCulculation msol1 msol2 dmpc ifo flower = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ snrInspiralCore msol1 msol2 dmpc ifolist2 flower

-- snrRingdownCulculation :: Double -> Double -> FilePath -> IO()
-- snrRingdownCulculation msol dmpc ifo = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ snrRingdown msol dmpc ifolist2

-- snrRingdownCoreCulculation :: Double -> Double -> Double -> Double -> Double -> FilePath -> Double -> Double -> IO()
-- snrRingdownCoreCulculation msol dmpc a epsil phi ifo fupp flower = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ snrRingdownCore msol dmpc a epsil phi ifolist2 fupp flower

-- tuplify2 ::  [a] ->  (a,a)
-- tuplify2 [x,y] = (x,y)
