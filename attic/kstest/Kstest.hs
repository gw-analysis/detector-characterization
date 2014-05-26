{-******************************************
  *     File Name: Kstest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/23 22:46:08
  *******************************************-}

{- Reference
- Numerical Recipes in C
    - ksone : p.461
    - probks: p.462
- GNU Sientific Library
    - gslCdfGaussinaP: http://www.gnu.org/software/gsl/manual/html_node/The-Gaussian-Distribution.html#The-Gaussian-Distribution
-}

module Kstest(ksone
             ,probks
             ) where

{-  GSL function  -}
import qualified Foreign.C.Types as FCT
foreign import ccall "gsl_cdf_gaussian_P" gsl_cdf_gaussian_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfGaussianP :: Double -> Double -> Double
gslCdfGaussianP x sigma = realToFrac $ gsl_cdf_gaussian_P (realToFrac x) (realToFrac sigma)


{-  Supplementary function  -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [ a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

mean :: [Double] -> Double
mean xData = (sum xData) / (fromIntegral $ length xData)

sdev :: [Double] -> Double
sdev xData = sqrt $ (sum [ (x - xMean)**2 | x <- xData]) / (fromIntegral $ length xData)
  where xMean = (sum xData) / (fromIntegral $ length xData)


{-  Core code  -}
-- KS統計量Dを引数に有為確率alphaを返す
probks :: Double -> Double
probks alam = sum xs
  where xs = [(-1.0)**(idx-1) * 2.0 * exp(-2.0*alam*alam*idx*idx) | idx <- [1,2..100]]

-- 累積密度F(x_i)を引数にKS統計量Dを返す
calcKS :: [Double] -> Double
calcKS funcData = maximum $ (map abs $ zipWith (-) funcData ys) ++ (map abs $ zipWith (-) (0:funcData) ys)
  where ys = [(fromIntegral idx)/(fromIntegral $ length funcData) | idx <- [0..(length funcData)]]

-- 累積密度F(x_i)を引数に規格化されたKS統計量sqrt(N)Dを返す
ksone :: [Double] -> Double
ksone funcData = ( en + 0.12 + (0.11/en) ) * (calcKS funcData)
  where en = sqrt $ fromIntegral $ length funcData

-- サンプルX_{i}を引数に規格化されたKS統計量
ksoneGauss :: [Double] -> Double
ksoneGauss xData = ksone $ map (flip gslCdfGaussianP xSdev) $ quicksort $ map ((-) xMean) xData
  where xSdev = sdev xData
        xMean = mean xData

{--  Test code  --}
-- main :: IO ()
-- main = do
--      let xData = [ 0.133919, -0.088101, 1.67441, 0.733641, 0.997525, -1.2775, -2.39672, -0.67928, -0.0390913, 0.893556
--                  , -0.0176478, -1.29656, -0.667981, 0.181709, 0.831051, -0.548248, -0.638032, 0.00708853, -0.669854, -0.827762
--                  , 0.6125, -0.367588, -0.289493, -0.916875, -0.583805, 0.0646153, 0.0467439, -0.750727, -0.144233, 1.47556
--                  , -0.206479, -0.260697, -0.138218, -2.5434, 0.577419, -0.0361099, -0.226173, 0.715002, 1.57727, -0.392274
--                  , 1.18277, 1.79311, -0.731346, -0.629818, 2.07069, -0.903006, 1.73555, 1.38309, -0.375367, -0.30431 ] :: [Double]
--      print $ ksoneGauss xData
--      print $ probks.ksoneGauss $ xData
{-
- Compilation
    > ghc --make -O Kstest Kstest.hs -lgsl -lgslcblas -lm
- Usage:
    > ./Kstest
    0.9487863343306464
    0.3289778781527417
-}


