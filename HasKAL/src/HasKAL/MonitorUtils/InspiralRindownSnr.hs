 ---テストコード
 -- snrInspiral
 -- *SpiralorRingdownSnr> snrInspiral 1.4 1.4 280 KAGRA 30 0.05
 -- fromList [8.168393032812842]

 -- snrRingdown
 -- *SpiralorRingdownSnr> snrRingdown 300 8697 0.9 0.01 0 KAGRA 4048 10 0.05
 -- fromList [8.667669341182771]

 ---出典元
 -- snrInspiral:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B3) 注:LIGOとKAGRAとのSNRの定義の違いにより、(B3)に対し1/sqrt(2)倍の補正がかかっています。
 -- snrRingdown:P.Ajith et al. Phys.Rev.D77:104017 (2008) 式番号(B10)


module HasKAL.MonitorUtils.InspiralRindownSnr
       (snrInspiral
       ,snrRingdown
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


 --- snrInspiralPow2:インスパイラルの積分部分を定義
 --- 数値積分においては、単純な\sum dfを使用
 -- 引数
 -- 連星質量1[太陽質量] 連星太陽質量2[太陽質量] 使用する検出器 周波数cutoff下限[Hz] 数値積分刻み幅
snrInspiralPow2 :: Vector Double ->  Vector Double -> Detector ->  Vector Double -> Vector Double -> Vector Double
snrInspiralPow2 msol1 msol2 ifo flower df
    | flower <  0 =  error "lower frequency : Why did you insert a minus number?"
    | df <  0 =  error "df : Why did you insert a minus number?"
    | flower > fupp =  0
    | otherwise = (integratedInspiral ifo flower)*df + (snrInspiralPow2 msol1 msol2 ifo (flower +  df)  df)
  where fupp = 1/((6**(3/2)) *pi*(msol1 + msol2)*(msolar)*(g/(c**3)))


 --- snrInspiral:インスパイラルのSNRを定義
 -- 引数
 -- 連星質量1[太陽質量] 連星質量2[太陽質量] 連星までの距離[Mpc] 使用する検出器 周波数cutoff下限[Hz] 数値積分刻み幅
snrInspiral :: Vector Double ->  Vector Double ->  Vector Double -> Detector ->  Vector Double -> Vector Double -> Vector Double
snrInspiral msol1 msol2 dmpc ifo flower df
    | msol1 <  0 =  error "mass 1: Why did you insert a minus number?"
    | msol2 <  0 =  error "mass 2 : Why did you insert a minus number?"
    | dmpc <  0 =  error "distance : Why did you insert a minus number?"
    | otherwise = ((2)**( - 0.5))*(cons*(allmass**(5/6))*((5*symmass/6)**(1/2))/(d * pi**(2/3)))*((snrInspiralPow2 msol1 msol2 ifo flower df)**(1/2))
  where allmass = (msol1 + msol2)*(msolar)
        symmass = msol1*msol2/((msol1 + msol2)**2)
        d = dmpc*(megapc)
        cons = c*((g/(c**3))**(5/6))


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


 --- snrRingdown:リングダウンのSNRを定義
 -- 引数
 -- BH質量[太陽質量] BHまでの距離[Mpc] Kerr parameter 質量欠損比率 初期位相 使用する検出器 周波数cutoff上限[Hz] 周波数cutoff下限[Hz] 数値積分刻み幅
snrRingdown :: Vector Double ->  Vector Double ->  Vector Double -> Vector Double -> Vector Double ->  Detector ->  Vector Double -> Vector Double -> Vector Double -> Vector Double
snrRingdown msol dmpc a epsil phi ifo fupp flower df
    | msol <  0 =  error "mass : Why did you insert a minus number?"
    | dmpc <  0 =  error "distance : Why did you insert a minus number?"
    | a <  0 =  error "Kerr parameter : Why did you insert a minus number?"
    | eps <  0 =  error " : Why did you insert a minus number?"
    | fupp <  0 =  error "upper frequency : Why did you insert a minus number?"
    | flower <  0 =  error "lower frequency : Why did you insert a minus number?"
    | df <  0 =  error "df : Why did you insert a minus number?"
    | otherwise =((((10*epsil*m*fqnr)/(q*pi*(1 + 7/(24*(q**2)))))**(1/2))*(snrRingdownPow2 msol a phi ifo fupp flower df)**(1/2))/(pi*q*(d/c))
  where m = (g*msol*(msolar)/(c**(3)))
        q = 2*((1 - a)**(-9/20))
        d = dmpc*(megapc)
        fqnr = (1 - 0.63*(1 - a)**(3/10))/(2*pi*m)
