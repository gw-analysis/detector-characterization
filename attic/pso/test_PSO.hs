-- module TestPSO
-- ( maxWindx
-- , maxList
-- ) where
import Control.Monad.State
import System.Random
import System.IO (hClose, hPutStrLn, openFile, stdout, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)

type Particle = ((Int,  Int),  [Double],  [Double],  [Double],  Double)
type GlobalParticle = (Int, [Double], Double)
type Likelihood = [Double] -> Double
main :: IO ()
main = do
  let w = 0.1 :: Double
      c1 = 2.0 :: Double
      c2 = 2.0 :: Double
      d = 2 :: Int
      m = 10 :: Int
      i0 = 30 :: Int

      -- | create initial data
      initdata = map (go i0) [1..m]
        where
        go n i = ((n, i), x, v, p, lr)
          where
          x = map (\_->unsafePerformIO $ getStdRandom $ randomR (-5, 5) :: Double) [1..d]
          v = map (\_->unsafePerformIO $ getStdRandom $ randomR (-1, 1) :: Double) [1..d]
          lr = likelihood x
          p = x
      (a,b) = pso i0 m d w c1 c2 initdata likelihood
      xx = [y | (x,y,z)<-b]
      aa = [y | (_,y,_,_,_)<-a]

  mapM_ (\yy-> hPutStrLn stdout $ show (yy!!0)++" "++show (yy!!1)) xx 
  h1 <- openFile "globalParticle.dat" WriteMode
  mapM_ (\yy-> hPutStrLn h1 $ show (yy!!0)++" "++show (yy!!1)) xx 
  hClose h1





-- | likelihood function
likelihood :: [Double] -> Double
likelihood x = -(((x!!0)-0.5)**2 + ((x!!1)-0.5)**2)

-- | perform particle swarm optimization
pso :: Int    -- ^ Max iteration number
    -> Int    -- ^ # of particles
    -> Int    -- ^ degrees of parameter space
    -> Double -- ^ inertia weight parameter
    -> Double -- ^ cognitive weight
    -> Double -- ^ social weight
    -> [Particle] -- ^ particle data
    -> Likelihood
    -> ([Particle], [GlobalParticle])
       -- ^ output : updated particle data
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
        g <- get
        let phist = for [1..m] $ \i -> do
             let (_, x, v, p, _) = singleList [ ((n', j), a, b, c, d)
                                              | ((n', j), a, b, c, d)<-pdata, j==i, n'==n
                                              ]
             let g' = snd' (g!!0)
                 updatedv = map (\i -> do
                   let r1 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                       r2 = unsafePerformIO $ getStdRandom $ randomR (0, 1) :: Double
                    in w*v!!i + c1*r1*(p!!i-x!!i) + c2*r2*(g'!!i-x!!i)
                   )[0..d-1]
                 updatedx = map (\i -> x!!i + updatedv!!i) [0..d-1]
                 newl = likelihood updatedx
                 (_, _, _, updatedp, _) = maxList [pdata!!0, ((n, i),updatedx,updatedv,updatedx,newl)]
              in ((n-1, i), updatedx, updatedv, updatedp, newl)
        let (_, updatedgval, _, _, updatedgl) = maxList (phist++pdata)
        put ((n-1, updatedgval, updatedgl):g)
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



