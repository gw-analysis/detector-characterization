-- compilation
-- ghc -O2 test.hs -rtsopts -threaded
-- execution
-- ./test +RTS -N3 -s


import Numeric.GSL.Fourier
import Numeric.LinearAlgebra
import Data.Complex
import Control.Monad
import Data.Time.Clock
import Control.DeepSeq
import Control.Concurrent.Async


main = do
  let xr = [1..64] :: [Double]
      x1 = [(y:+0) | y <- xr]
      vx1 = fromList x1 :: Vector (Complex Double)
      x2 = [(y:+0) | y <- xr]
      vx2 = fromList x2 :: Vector (Complex Double)
      x3 = [(y:+0) | y <- xr]
      vx3 = fromList x3 :: Vector (Complex Double)
      x4 = [(y:+0) | y <- xr]
      vx4 = fromList x4 :: Vector (Complex Double)

  let listv = [vx1, vx2, vx3, vx4, vx1,  vx2,  vx3,  vx4, vx1,  vx2,  vx3,  vx4]
  listv `deepseq` return ()


  t0 <- getCurrentTime

  jobs <- mapM (\v -> async (return $ fft v)) listv
  mapM wait jobs

  t1 <- getCurrentTime

  print $ diffUTCTime t1 t0













