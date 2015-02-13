-- module TestPSO
-- ( maxWindx
-- , maxList
-- ) where

import System.Random
import System.IO.Unsafe (unsafePerformIO)

main :: IO([((Int, Int), [Double], [Double], [Double], Double)])
main = do

  let w = 0.5 :: Double
      c1 = 1.0 :: Double
      c2 = 1.0 :: Double
      d = 2 :: Int
      m = 3 :: Int
      i0 = 10 :: Int

      -- | create initial data
      initdata = map (go i0) [1..m]
        where
        go n i = ((n, i), x, v, p, lr)
          where
          x = map (\_->unsafePerformIO $ getStdRandom $ randomR (-5, 5) :: Double) [1..d]
          v = map (\_->unsafePerformIO $ getStdRandom $ randomR (-1, 1) :: Double) [1..d]
          lr = likelihood x
          p = x
      (_, g, _, _, _) = maxList initdata

  return $ pso i0 m d w c1 c2 initdata g

-- | perform particle swarm optimization
pso :: Int    -- ^ Max iteration number
    -> Int    -- ^ # of particles
    -> Int    -- ^ degrees of parameter space
    -> Double -- ^ inertia weight parameter
    -> Double -- ^ cognitive weight
    -> Double -- ^ social weight
    -> [((Int, Int), [Double], [Double], [Double], Double)] -- ^ particle data
    -> [Double] -- ^ local best position of each particle
    -> [((Int, Int), [Double], [Double], [Double], Double)] -- ^ output : updated particle data
pso 0 _ _ _ _ _ _ _ = []
pso i0 m d w c1 c2 pdata g = do
  let updatedpdata = (map (update (i0-1) pdata) [1..m])
      update n dat i = ((n, i), updatedx, updatedv, updatedp, newl)
        where
        (_, x, v, p, _) = singleList [((n, i), a, b, c, d)|((n', i'), a, b, c, d)<-dat,i'==i, n'==n]
        updatedv = map (\i->do
                       let r1 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                           r2 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                        in w*v!!i + c1*r1*(p!!i-x!!i) + c2*r2*(g!!i-x!!i))
                        [0..d-1]
        updatedx = map (\i->x!!i + updatedv!!i) [0..d-1]
        newl = likelihood updatedx
        (_, _, _, updatedp, _) = maxList [((n', ind), a, b, c, d)
                            |((n', ind), a, b, c, d) <- updatedpdata, ind==i
                            ]
      (_,  updatedg,  _,  _, _) = maxList updatedpdata
   in  updatedpdata ++ pso (i0-2) m d w c1 c2 updatedpdata updatedg

-- | likelihood function
likelihood :: [Double] -> Double
likelihood x = ((x!!0)-0.5)**2 + ((x!!1)-0.5)**2

-- | get (max value, index) from comaring two values
maxWindx :: ((Int, Int), [Double], [Double], [Double], Double)
         -> ((Int, Int), [Double], [Double], [Double], Double)
         -> ((Int, Int), [Double], [Double], [Double], Double)
maxWindx (sa, xa, va, pa, a) (sb, xb, vb, pb, b)
  | a > b = (sa, xa, va, pa, a)
  | a < b = (sb, xb, vb, pb, b)
  | a == b = (sa, xa, va, pa, a)
  | otherwise = error "check input type"

-- | get (max value, index) in a list
maxList :: [((Int, Int), [Double], [Double], [Double], Double)]
        -> ((Int, Int), [Double], [Double], [Double], Double)
maxList [] = error "empty"
maxList [x] = x
maxList (x:xs) = maxWindx x (maxList xs)

-- | one element list to a value
singleList :: [((Int, Int), [Double], [Double], [Double], Double)]
           -> ((Int, Int), [Double], [Double], [Double], Double)
singleList [x] = x


