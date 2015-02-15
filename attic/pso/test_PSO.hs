-- module TestPSO
-- ( maxWindx
-- , maxList
-- ) where
import Control.Monad.State
import System.Random
import System.IO.Unsafe (unsafePerformIO)

type Particle = ((Int,  Int),  [Double],  [Double],  [Double],  Double)
type GlobalParticle = (Int, [Double])


main :: IO ([Particle], [GlobalParticle])
main = do
  let w = 1 :: Double
      c1 = 2.0 :: Double
      c2 = 2.0 :: Double
      d = 2 :: Int
      m = 3 :: Int
      i0 = 3 :: Int

      -- | create initial data
      initdata = map (go i0) [1..m]
        where
        go n i = ((n, i), x, v, p, lr)
          where
          x = map (\_->unsafePerformIO $ getStdRandom $ randomR (-5, 5) :: Double) [1..d]
          v = map (\_->unsafePerformIO $ getStdRandom $ randomR (-1, 1) :: Double) [1..d]
          lr = likelihood x
          p = x
  return $ pso i0 m d w c1 c2 initdata



-- | perform particle swarm optimization
pso :: Int    -- ^ Max iteration number
    -> Int    -- ^ # of particles
    -> Int    -- ^ degrees of parameter space
    -> Double -- ^ inertia weight parameter
    -> Double -- ^ cognitive weight
    -> Double -- ^ social weight
    -> [Particle] -- ^ particle data
    -> ([Particle], [GlobalParticle])
       -- ^ output : updated particle data
pso n m d w c1 c2 pdata = runState go g
  where
    (_, gval, _, _, _) = maxList pdata
    g = [(n, gval)]
    go = psoState n m d w c1 c2 pdata  :: State [GlobalParticle] [Particle]

psoState :: Int
         -> Int
         -> Int
         -> Double
         -> Double
         -> Double
         -> [Particle]
         -> State [GlobalParticle] [Particle]
psoState n m d w c1 c2 pdata = do
  g <- get
  let phist= concat $ for [n, n-1..0] $ \n' -> do
      concat $ for [1..m] $ \i -> do
        let (_, x, v, p, _) = singleList [ ((n, j), a, b, c, d)
                                         | ((n, j), a, b, c, d)<-pdata, j==i, n==n'
                                         ]
        let g' = snd (g!!0)
            updatedv = map (\i->do
              let r1 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                  r2 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
               in w*v!!i + c1*r1*(p!!i-x!!i) + c2*r2*(g'!!i-x!!i)
              )[0..d-1]
            updatedx = map (\i->x!!i + updatedv!!i) [0..d-1]
            newl = likelihood updatedx
            (_, _, _, updatedp, _) = maxList [pdata!!0, ((n', i),updatedx,updatedv,updatedx,newl)]
            values = ((n', i), updatedx, updatedv, updatedp, newl)
        (values:pdata)
  let (_, updatedgval, _, _, _) = maxList phist
  put ((n, updatedgval):g)
  state $ \s ->  (phist, s)


-- | define for
for = flip map


-- | likelihood function
likelihood :: [Double] -> Double
likelihood x = ((x!!0)-0.5)**2 + ((x!!1)-0.5)**2

-- | get (max value, index) from comaring two values
maxWindx :: Particle
         -> Particle
         -> Particle
maxWindx (sa, xa, va, pa, a) (sb, xb, vb, pb, b)
  | a > b = (sa, xa, va, pa, a)
  | a < b = (sb, xb, vb, pb, b)
  | a == b = (sa, xa, va, pa, a)
  | otherwise = error "check input type"

-- | get (max value, index) in a list
maxList :: [Particle]
        -> Particle
maxList [] = error "empty"
maxList [x] = x
maxList (x:xs) = maxWindx x (maxList xs)

-- | one element list to a value
singleList :: [Particle]
           -> Particle
singleList [] = error "empty"
singleList [x] = x
singleList _ = error "not correct"

