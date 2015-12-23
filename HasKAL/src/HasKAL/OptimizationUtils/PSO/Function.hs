module HasKAL.OptimizationUtils.PSO
( pso
--,
) where


import Control.Monad.State
import System.Random
import System.IO.Unsafe (unsafePerformIO)

type Position = [Double]
type Velocity = [Double]
type LocalBest= [Double]
type Iteration= Int
type ParticleID=Int
type Particle = ((Iteration, ParticleID), Position, Velocity, LocalBest, Double)
type GlobalParticle = (Iteration, Position, Double)
type Likelihood = Position -> Double


-- | perform particle swarm optimization
pso :: Int    -- ^ Max iteration number
    -> Int    -- ^ # of particles
    -> Int    -- ^ degrees of parameter space
    -> Double -- ^ inertia weight parameter
    -> Double -- ^ cognitive weight parameter
    -> Double -- ^ social weight parameter
    -> [Particle] -- ^ particle data
    -> Likelihood -- ^ likelihood function :: Position->Double
    -> ([Particle], [GlobalParticle]) -- ^ output : updated particle data
pso n m d w c1 c2 pdata likelihood = runState go g
  where
    (_, gval, _, _, gl) = maxList pdata
    g = [(n, gval, gl)] :: [GlobalParticle]
    go = psoState n m d w c1 c2 pdata likelihood

psoState :: Int
         -> Int
         -> Int
         -> Double
         -> Double
         -> Double
         -> [Particle]
         -> Likelihood
         -> State [GlobalParticle] [Particle]
psoState n m d w c1 c2 pdata likelihood = do
  loop n m d w c1 c2 pdata likelihood
    where
      loop 0 _ _ _ _ _ _ _ = return []
      loop n m d w c1 c2 pdata likelihood = do
        -- | obtain State value
        g <- get
        let phist = for [1..m] $ \i -> do
             let (_, x, v, p, _) = singleList [ ((n', j), a, b, c, d)
                                              | ((n', j), a, b, c, d)<-pdata, j==i, n'==n
                                              ]
             let g' = snd' (g!!0)
                 updatedv = map (\i -> do
                   let r1 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                       r2 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                    -- | velocity update rule
                    in w*v!!i + c1*r1*(p!!i-x!!i) + c2*r2*(g'!!i-x!!i)
                   )[0..d-1]
                 -- | position udate rule
                 updatedx = map (\i -> x!!i + updatedv!!i) [0..d-1]
                 newl = likelihood updatedx
                 -- | update local best particle
                 (_, _, _, updatedp, _) = maxList [pdata!!0, ((n, i),updatedx,updatedv,updatedx,newl)]
              in ((n-1, i), updatedx, updatedv, updatedp, newl)
        -- | update global best particle
        let (_, updatedgval, _, _, updatedgl) = maxList (phist++pdata)
        -- | update State value
        put ((n-1, updatedgval, updatedgl):g)
        -- | iteration-termination threshold
        case (abs updatedgl < 0.1) of
          True -> return $ phist ++ pdata
          False -> do
            nextphist <- loop (n-1) m d w c1 c2 (phist++pdata) likelihood
            return $ nextphist ++ phist

-- | define for
for = flip map

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

-- | three element tuple
fst' :: (a, b, c) -> a
fst' (x, _, _) = x
snd' :: (a, b, c) -> b
snd' (_, y, _) = y
thd' :: (a, b, c) -> c
thd' (_, _, z) = z
