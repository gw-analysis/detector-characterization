module FFT (
  fft'c2c,
  fft'r2c,
  ifft'c2c,
  ifft'c2r
) where

import Data.Vector.Storable
import Prelude hiding ((++), map, length, init, tail, reverse, take)
import Data.Complex (Complex(..), conjugate, realPart)
import Numeric.GSL.Fourier (fft, ifft)

{--  Test Code --}
-- main = do
--   putStrLn "--- Original"
--   let xv = fromList [4.0, 3, 2, 1]
--   forM_ xv $ \x -> print x
  
--   putStrLn "\n--- FFT"
--   let yv = fft'r2c xv
--   forM_ yv $ \y -> print y

--   putStrLn "\n--- IFFT"
--   let zv = ifft'c2r yv
--   forM_ zv $ \z -> print z
{-- test result
--- Original
4.0
3.0
2.0
1.0

--- FFT
10.0 :+ 0.0
2.0 :+ (-2.0)
2.0 :+ 0.0

--- IFFT
4.0
3.0
2.0
1.0
--}

{-- External Function --}
-- | FFT function
fft'c2c :: Vector (Complex Double) -- ^ Length: N
        -> Vector (Complex Double) -- ^ Length: N
fft'c2c = fft

-- | FFT function
fft'r2c :: Vector Double           -- ^ Length: N
        -> Vector (Complex Double) -- ^ Length: N/2 + 1
fft'r2c xv = take (length xv `div` 2 + 1) $ fft $ map toComplex xv
  where toComplex x = x :+ 0

-- | IFFT function
ifft'c2c :: Vector (Complex Double) -- ^ Length: N
         -> Vector (Complex Double) -- ^ Length: N
ifft'c2c = ifft

-- | IFFT function
ifft'c2r :: Vector (Complex Double) -- ^ Length: N/2 + 1
         -> Vector Double           -- ^ Length: N
ifft'c2r xv = map realPart $ ifft $ xv ++ negativeFreq xv
  where negativeFreq = map conjugate.reverse.init.tail

