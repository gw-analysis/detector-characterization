 --- テストコード
 -- 注:このソースコード自体にIOは含まれていないので、ここに書いてあるテストコードはこのプログラムの一番下にあるIOプログラム群を通して出力したものになっています。ご了承下さい。
 --  distInspiralCore
 --  *Main> distInspiralCoreCulculation 1.4 1.4 8 "kagraPsd.dat" 30
 --  286.01470379660367

 --  distInspiral
 --  *Main ->  distInspiralCulculation 1.4 1.4 8 "kagraPsd.dat"
 --  286.01470379660367

 --  distRingdownCore
 --  *Main ->  distRingdownCoreCulculation 300 8 0.9 0.01 0 "kagraPsd.dat" 2048 10
 --  9422.839968026909

 --  distRingdown
 --  *Main> distRingdownCulculation  300 8 0.9 0.01 0 "kagraPsd.dat"
 --  9422.819838719484""

 --- 出典元
 --  snrInspiral:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B3) 注:LIGOとKAGRAとのSNRの定義の違いにより、(B3)に対し1/sqrt(2)倍の補正がかかっています。
 --  snrRingdown:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B10)



module HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta
       (distRingdownCore
       ,distRingdown
       ,distInspiralCore
       ,distInspiral)where

import Numeric.LinearAlgebra
import Data.List


 -- 光速の定義[m/s]
c :: Double
c  = 2.99*(10**8)

 -- 万有引力定数の定義[m^3 kg^-1 s^-2]
g :: Double
g = 6.67*(10**( - 11))

 -- 太陽質量[kg]
msolar :: Double
msolar = 1.989*(10**30)

 -- Mpc[m]
megapc :: Double
megapc = 3.085677*(10**22)


 --- distInspiral:インスパイラルの距離を計算する準備をし距離を返す
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] SNR 使用する検出器の[(周波数,ノイズパワースペクトル)]データ
distInspiral :: Double -> Double -> Double -> [(Double,Double)] -> Double
distInspiral  msol1 msol2 snr ifo = getsnrInspiral
  where ifonontupl = map invtuplify2 ifo
        fupp =  1/((6**(3/2)) *pi*(msol1 +  msol2)*(msolar)*(g/(c**3)))
        flower = 30
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
        getsnrInspiral = distInspiralculc msol1 msol2 snr snrRingdownPow2 -- 距離を計算する


 --- snrInspiralCore:インスパイラルの距離を計算する準備をし、周波数カットオフもパラメータに加えた上で距離を返す
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] 連星までの距離[Mpc] 使用する検出器の[(周波数,ノイズパワースペクトル)]データ 周波数cutoff下限[Hz]
distInspiralCore :: Double -> Double -> Double -> [(Double,Double)] -> Double -> Double
distInspiralCore  msol1 msol2 snr ifo flower = getsnrInspiral
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
        getsnrInspiral = distInspiralculc msol1 msol2 snr snrRingdownPow2 -- SNRを計算する


 --- integratedInspiral:インスパイラルの被積分関数を定義
 -- 引数
 -- 使用する検出器の(周波数,パワースペクトル)データの 周波数 及び パワースペクトル
integratedInspiral :: Double -> Double -> Double
integratedInspiral fin noiseSpec =  fin**( - 7/3)/noiseSpec


 --- distInspiralculc:インスパイラルの距離を定義
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] SNR インスパイラル被積分関数の和
distInspiralculc :: Double -> Double -> Double -> Double -> Double
distInspiralculc msol1 msol2 snr snrInspiralPow2= ((2)**( -  0.5))*(cons*(allmass**(5/6))*((5*symmass/6)**(1/2))/(snrstand * pi**(2/3)))*(snrInspiralPow2**(1/2))
  where allmass =  (msol1 +  msol2)*(msolar)
        symmass =  msol1*msol2/((msol1 +  msol2)**2)
        snrstand =  snr*(megapc)
        cons =  c*((g/(c**3))**(5/6))


 --- distRingdown:リングダウンの距離を計算する準備をし、距離を返す
 -- 引数
 -- BH質量[太陽質量] SNR Kerr parameter 質量欠損比率 初期位相 使用する検出器のリストデータ 周波数cutoff上限[Hz] 周波数cutoff下限[Hz]
distRingdown :: Double -> Double -> Double -> Double -> Double -> [(Double,Double)] -> Double 
distRingdown msol snr a epsil phi ifo = getsnrRingdown
  where flower = 30
        fupp = 2048
        ifonontupl = map invtuplify2 ifo
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
        getsnrRingdown = distRingdownculc msol snr a epsil snrRingdownPow2 -- 距離を計算する

 --- distRingdownCore:リングダウンの距離を計算する準備をし、周波数カットオフもパラメータに加えた上で距離を返す
 -- 引数
 -- BH質量[太陽質量] SNR Kerr parameter 質量欠損比率 初期位相 使用する検出器のリストデータ 周波数cutoff上限[Hz] 周波数cutoff下限[Hz]
distRingdownCore :: Double -> Double -> Double -> Double -> Double -> [(Double,Double)] -> Double -> Double -> Double
distRingdownCore  msol snr a epsil phi ifo fupp flower = getsnrRingdown
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
        getsnrRingdown = distRingdownculc msol snr a epsil snrRingdownPow2 -- 距離を計算する


 --- integratedRingdown:リングダウンの距離の被積分関数を定義
 -- 引数
 -- BH質量[太陽質量] Kerr parameter 使用する検出器 検出器のノイズスペクトル
integratedRingdown :: Double -> Double -> Double -> Double -> Double -> Double
integratedRingdown msol a phi fin noiseSpec = ((cos(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) + 1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2) + (sin(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) -  1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2)) /noiseSpec
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

-- distInspiralCulculation :: Double -> Double -> Double -> FilePath -> IO()
-- distInspiralCulculation msol1 msol2 snr ifo = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distInspiral msol1 msol2 snr ifolist2

-- distInspiralCoreCulculation :: Double -> Double -> Double -> FilePath -> Double -> IO()
-- distInspiralCoreCulculation msol1 msol2 snr ifo flower = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distInspiral msol1 msol2 snr ifolist2

-- distRingdownCulculation :: Double -> Double -> Double -> Double -> Double -> FilePath -> IO()
-- distRingdownCulculation msol snr a epsil phi ifo = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distRingdown msol snr a epsil phi ifolist2

-- distRingdownCoreCulculation :: Double -> Double -> Double -> Double -> Double -> FilePath -> Double -> Double -> IO()
-- distRingdownCoreCulculation msol snr a epsil phi ifo fupp flower = do
--   contents <- readFile ifo  -- FilePathのデータのポインタをcontentsに格納
--   let num =  map words $ lines contents      -- データを[[[周波数1,パワースペクトル1],[周波数2,パワースペクトル2]..],..]という形に整形
--       ifolist = map (map read) num :: [[Double]] -- データをDoubleに変換
--       ifolist2 = map tuplify2 ifolist -- データを[(Double,Double)]に変換
--   print $ distRingdownCore msol snr a epsil phi ifolist2 fupp flower


-- tuplify2 ::  [a] ->  (a,a)
-- tuplify2 [x,y] = (x,y)
