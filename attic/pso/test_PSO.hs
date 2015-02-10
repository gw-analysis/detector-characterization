-- module TestPSO
-- ( maxWindx
-- , maxList
-- ) where

import System.Random
import System.IO.Unsafe (unsafePerformIO)


main = do

  let w = 0.5 :: Double
      c1 = 1.0 :: Double
      c2 = 1.0 :: Double
      d = 2 :: Int
      m = 3 :: Int
      i0 = 10 :: Int

      initdata = map (go i0) [1..m]
        where
        go n i = ((n, i), x, v, lr)
        x = map (\_->unsafePerformIO $ getStdRandom $ randomR (-5, 5) :: Double) [1..d]
        v = map (\_->unsafePerformIO $ getStdRandom $ randomR (-1, 1) :: Double) [1..d]
        lr = likelihood x
      p = concat [x|(_, x, _, _) <- initdata]
      (_, g, _, _) = maxList initdata

  return $ pso i0 m d w c1 c2 initdata p g


pso 0 _ _ _ _ _ _ _ _ = []
pso i0 m d w c1 c2 pdata p g = do
  let updatedpdata = (map (update (i0-1) pdata) [1..m]) ++ pdata
      update n dat i = ((n, i), updatedx, updatedv, newl)
        where
        (_, x, v, _) = dat!!0
        updatedv = map (\i->do
                       let r1 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                           r2 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                        in w*v!!i + c1*r1*(p!!i-x!!i) + c2*r2*(g!!i-x!!i))
                        [0..d-1]
        updatedx = map (\i->x!!i + updatedv!!i) [0..d-1]
        newl = likelihood updatedx
      updatedp = concat $ map (\i->do
        let (_, x, _, _) = maxList [((a, ind), b, c, d)|((a, ind), b, c, d) <- updatedpdata, ind==i]
         in x)
         [1..m]
      (_,  updatedg,  _,  _) = maxList updatedpdata
  pso (i0-1) m d w c1 c2 updatedpdata updatedp updatedg

-- | likelihood function
-- |
likelihood :: [Double] -> Double
likelihood x = ((x!!0)-0.5)**2 + ((x!!1)-0.5)**2

-- | get (max value, index) from comaring two values
-- |
maxWindx :: ((Int, Int), [Double], [Double], Double)
         -> ((Int, Int), [Double], [Double], Double)
         -> ((Int, Int), [Double], [Double], Double)
maxWindx (sa, xa, va, a) (sb, xb, vb, b)
  | a > b = (sa, xa, va, a)
  | a < b = (sb, xb, vb, b)
  | a == b = (sa, xa, va, a)
  | otherwise = error "check input type"

-- | get (max value, index) in a list
-- |
maxList :: [((Int, Int), [Double], [Double], Double)]
        -> ((Int, Int), [Double], [Double], Double)
maxList [] = error "empty"
maxList [x] = x
maxList (x:xs) = maxWindx x (maxList xs)




