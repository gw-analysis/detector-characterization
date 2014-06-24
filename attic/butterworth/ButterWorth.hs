module ButterWorth
( butterPoly
--,
)
where


import FilterType

--butter :: Int -> Double -> FilterType -> Double
--butter n freq filterType
--  | filterType==Low = butterCore n a




{- Normalized ButterWorth polynomials -}
butterPoly :: Int -> Double -> Double
butterPoly n s
    | even n = foldl (\acc k->(s**2-2*s*cos((2*fromIntegral k+fromIntegral n-1)*pi/(2*fromIntegral n))+1)*acc) 0 [1..(n `div` 2)]
    | odd n  = (s+1) * (foldl (\acc k->(s**2-2*s*cos((2*fromIntegral k+fromIntegral n-1)*pi/(2*fromIntegral n))+1)*acc) 0 [1..((n-1) `div` 2)])

butterCore :: Int -> Double -> Double
butterCore n s = gain0 / butterPoly n s
  where gain0 = 1.0 :: Double

