 ---テストコード
 -- distInspiralCore
 -- *InspiralorRingdownDistance> distInspiralCore 1.4 1.4 8 KAGRA 30 0.05
 -- fromList [285.8937561484495]

 -- distInspiral
 -- *InspiralorRingdownDistance> distInspiral 1.4 1.4 8 KAGRA
 -- fromList [288.14704708275013]

 -- distRingdownCore
 -- *InspiralorRingdownDistance> distRingdownCore 300 8 0.9 0.01 0 KAGRA 4048 10 0.05
 -- fromList [9422.84003253332]

 -- distRingdown
 -- *InspiralorRingdownDistance> distRingdown 300 8 0.9 0.01 0 KAGRA
 -- fromList [9422.840032902668]

 ---出典元
 -- snrInspiral:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B3) 注:LIGOとKAGRAとのSNRの定義の違いにより、(B3)に対し1/sqrt(2)倍の補正がかかっています。
 -- snrRingdown:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B10)


module HasKAL.MonitorUtils.InspiralRingdownDistance
       (distInspiralCore
       ,distInspiral
       ,distRingdownCore
       ,distRingdown
       )where

import HasKAL.SpectrumUtils.DetectorSensitivity
import HasKAL.DetectorUtils.Detector
import Numeric.LinearAlgebra


 -- 光速の定義
c :: Vector Double
c  = 2.99*(10**8)

 -- 万有引力定数の定義
g :: Vector Double
g = 6.67*(10**( - 11))

 -- 太陽質量[kg]
msolar :: Vector Double
msolar = 1.989*(10**30)

 -- Mpc[m]
megapc :: Vector Double
megapc = 3.085677*(10**22)


 --- integratedInpiral:インスパイラルのSNRの被積分関数を定義
 -- 引数
 -- 使用する検出器 周波数
integratedInspiral :: Detector -> Vector Double -> Vector Double
integratedInspiral ifo fin = fin**(-7/3)/(ifonoisepsd ifo fin)


 --- distInspiralPow2:インスパイラルの積分部分を定義
 --- 数値積分においては、単純な\sum dfを使用
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] 使用する検出器 周波数cutoff下限[Hz] 数値積分刻み幅
distInspiralPow2 :: Vector Double ->  Vector Double -> Detector ->  Vector Double -> Vector Double -> Vector Double
distInspiralPow2 msol1 msol2 ifo flower df
    | flower <  0 =  error "lower frequency : Why did you insert a minus number?"
    | df <  0 =  error "df : Why did you insert a minus number?"
    | flower > fupp =  0
    | otherwise = (integratedInspiral ifo flower)*df + (distInspiralPow2 msol1 msol2 ifo (flower +  df)  df)
  where fupp = 1/((6**(3/2)) *pi*(msol1 + msol2)*(msolar)*(g/(c**3)))


 --- distInspiralCore:インスパイラルの距離[Mpc]を詳しいパラメータ入力でSNRから算出
 -- 引数
 -- 連星質量1[太陽質量] 連星質量2[太陽質量] SNR 使用する検出器 周波数cutoff下限[Hz] 数値積分刻み幅
distInspiralCore :: Vector Double ->  Vector Double ->  Vector Double -> Detector ->  Vector Double -> Vector Double -> Vector Double
distInspiralCore msol1 msol2 snr ifo flower df
    | msol1 <  0 =  error "mass 1: Why did you insert a minus number?"
    | msol2 <  0 =  error "mass 2 : Why did you insert a minus number?"
    | snr <  0 =  error "SNR : Why did you insert a minus number?"
    | otherwise = ((2)**( - 0.5))*(cons*(allmass**(5/6))*((5*symmass/6)**(1/2))/(snrstand * pi**(2/3)))*((distInspiralPow2 msol1 msol2 ifo flower df)**(1/2))
  where allmass = (msol1 + msol2)*(msolar)
        symmass = msol1*msol2/((msol1 + msol2)**2)
        snrstand = snr*(megapc)
        cons = c*((g/(c**3))**(5/6))


 --- distInspiral:インスパイラルの距離[Mpc]を簡易なパラメータ入力でSNRから算出
 -- 引数
 -- 連星質量1[太陽質量] 連星質量2[太陽質量] SNR 使用する検出器
distInspiral :: Vector Double ->  Vector Double ->  Vector Double -> Detector ->  Vector Double
distInspiral msol1 msol2 snr ifo
    | msol1 <  0 =  error "mass 1: Why did you insert a minus number?"
    | msol2 <  0 =  error "mass 2 : Why did you insert a minus number?"
    | snr <  0 =  error "SNR : Why did you insert a minus number?"
    | otherwise = ((2)**( - 0.5))*(cons*(allmass**(5/6))*((5*symmass/6)**(1/2))/(snrstand * pi**(2/3)))*((distInspiralPow2 msol1 msol2 ifo flower df)**(1/2))
  where allmass = (msol1 + msol2)*(msolar)
        symmass = msol1*msol2/((msol1 + msol2)**2)
        snrstand = snr*(megapc)
        cons = c*((g/(c**3))**(5/6))
        flower = 10
        df = 0.1


 --- integratedRingdown:リングダウンのSNRの被積分関数を定義
 -- 引数
 -- BH質量[太陽質量] Kerr parameter 使用する検出器 周波数
integratedRingdown :: Vector Double -> Vector Double -> Vector Double -> Detector -> Vector Double -> Vector Double
integratedRingdown msol a phi ifo fin = ((cos(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) + 1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2) + (sin(phi)**2)*((1/((fqnr/q)**2 + 4*(fin - fqnr)**2) -  1/((fqnr/q)**2 + 4*(fin + fqnr)**2))**2)) /(ifonoisepsd ifo fin)
  where q = 2*(1 - a)**( - 9/20)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)
          where m = (g*msol*(msolar)/(c**(3)))


 --- snrRingdownPow2:リングダウンの積分部分を定義
 --- 数値積分においては、単純な\sum dfを使用
 -- 引数
 -- BH質量[太陽質量] Kerr parameter 使用する検出器 周波数cutoff上限[Hz] 周波数cutoff下限[Hz] 数値積分刻み幅
snrRingdownPow2 :: Vector Double -> Vector Double ->  Vector Double -> Detector ->  Vector Double -> Vector Double -> Vector Double -> Vector Double
snrRingdownPow2 msol a phi ifo fupp flower df
    | flower > fupp =  0
    | otherwise = (integratedRingdown msol a phi ifo flower)*df + (snrRingdownPow2 msol a phi ifo fupp (flower +  df)  df)


 --- distRingdownCore:リングダウンの距離をSNRから詳細なパラメータで算出
 -- 引数
 -- BH質量[太陽質量] SNR Kerr parameter 質量欠損比率 初期位相 使用する検出器 周波数cutoff上限[Hz] 周波数cutoff下限[Hz] 数値積分刻み幅
distRingdownCore :: Vector Double ->  Vector Double ->  Vector Double -> Vector Double -> Vector Double ->  Detector ->  Vector Double -> Vector Double -> Vector Double -> Vector Double
distRingdownCore msol snr a epsil phi ifo fupp flower df
    | msol <  0 =  error "mass : Why did you insert a minus number?"
    | snr <  0 =  error "SNR : Why did you insert a minus number?"
    | a <  0 =  error "Kerr parameter : Why did you insert a minus number?"
    | eps <  0 =  error " : Why did you insert a minus number?"
    | fupp <  0 =  error "upper frequency : Why did you insert a minus number?"
    | flower <  0 =  error "lower frequency : Why did you insert a minus number?"
    | df <  0 =  error "df : Why did you insert a minus number?"
    | otherwise =((((10*epsil*m*fqnr)/(q*pi*(1 + 7/(24*(q**2)))))**(1/2))*(snrRingdownPow2 msol a phi ifo fupp flower df)**(1/2))/(pi*q*(snrstand/c))
  where m = (g*msol*(msolar)/(c**(3)))
        q = 2*((1 - a)**(-9/20))
        snrstand = snr*(megapc)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)


 --- distRingdown:リングダウンの距離をSNRから簡易なパラメータで算出
 -- 引数
 -- BH質量[太陽質量] SNR Kerr parameter 質量欠損比率 初期位相 使用する検出器
distRingdown :: Vector Double ->  Vector Double ->  Vector Double -> Vector Double -> Vector Double ->  Detector ->  Vector Double
distRingdown msol snr a epsil phi ifo
    | msol <  0 =  error "mass : Why did you insert a minus number?"
    | snr <  0 =  error "SNR : Why did you insert a minus number?"
    | a <  0 =  error "Kerr parameter : Why did you insert a minus number?"
    | eps <  0 =  error " : Why did you insert a minus number?"
    | otherwise =((((10*epsil*m*fqnr)/(q*pi*(1 + 7/(24*(q**2)))))**(1/2))*(snrRingdownPow2 msol a phi ifo fupp flower df)**(1/2))/(pi*q*(snrstand/c))
  where m = (g*msol*(msolar)/(c**(3)))
        q = 2*((1 - a)**(-9/20))
        snrstand = snr*(megapc)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)
        fupp = 4048
        flower = 10
        df = 0.1
