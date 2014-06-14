module HasKAL.SignalProcessing.WindowFunction
( windowed
, hanning)
where

import Numeric.LinearAlgebra

windowed :: Vector Double -> Vector Double -> Vector Double
windowed w x = w * x

hanning :: Int -> Vector Double
hanning = makeWindow hanning'

makeWindow :: (Double -> Double -> Double) -> Int -> Vector Double
makeWindow win m =
    let md = fromIntegral m
    in fromList $ map (win md . fromIntegral) [(0::Int)..(m-1::Int)]

hanning' :: Double -> Double -> Double
hanning' m n = 0.5 - 0.5 * cos(2 * pi * n / m)


