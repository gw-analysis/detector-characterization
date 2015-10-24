


module HasKAL.SignalProcessingUtils.Parallel
( tf2cparallel
, tf2rparallel
, iirp
) where


import qualified Control.Monad.Par.Scheds.Trace as Par
import qualified Control.Monad.Par as Par
import qualified Control.Monad.Par.Combinator as Par
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as VS
import HasKAL.MathUtils.LinearAlgebra.Function (polyval, toeplitz)
import HasKAL.SignalProcessingUtils.Filter (fir, sos1filter)
import Numeric.LinearAlgebra
import Numeric.GSL.Polynomials(polySolve)


tf2rparallel :: ([Double], [Double]) -> ([Double], [([Double], [Double])])
tf2rparallel (num, denom) =
  let (c, gain, alpha) = tf2cparallel (num, denom)
   in (c, func gain alpha)
   where
     func :: [Complex Double] -> [Complex Double] -> [([Double], [Double])]
     func _ [] = []
     func gain alpha =
       let hal = head alpha
           hA  = head gain
        in case (imagPart hal ==0) of
             False -> ([2*realPart hA, -2*realPart (hA * conjugate hal)]
               , [1, -2*realPart hal, realPart (abs hal)**2]) : func (drop 2 gain) (drop 2 alpha)
             True  -> ([realPart hA, 0], [1, -1*realPart hal, 0]) : func (tail gain) (tail alpha)


tf2cparallel :: ([Double], [Double]) -> ([Double], [Complex Double], [Complex Double])
tf2cparallel (num', denom') = do
  let num = map (/head denom') num'
      denom = map (/head denom') denom'
      p = length denom - 1
      q = length num - 1
      (c, d) = case (q >= p) of
        True ->
          let temp = toeplitz (denom ++ replicate (q-p) (0::Double))
                (head denom : replicate (q-p) (0::Double))
              tempM = fromColumns $ map fromList temp
              numM = ((q+1) >< 1) num
              denomM = ((p+1) >< 1) denom
              zeros = replicate (q-p+1) (fromList $ replicate p (0::Double))
              eye = ident p :: Matrix Double
              temp' = fromBlocks [[tempM, fromRows (toRows eye ++ zeros)]]
              temp''= temp' <\> numM
              c = toList . head . toColumns $ subMatrix (0, 0) (q-p+1, 1) temp''
              d' = toList . head . toColumns $ subMatrix (q-p+1, 0) (p, 1) temp''
           in (c, d2clist d')
        False ->
          let c = []
              d' = num ++ replicate (p-q-1) (0::Double)
           in (c, d2clist d')

  let alpha = polySolve $ reverse denom
      gpf = map go [0..length alpha -1]
        where
          go i =
            let scale = product [alpha!!i - x | x <- alpha, x /= alpha!!i]
             in polyval (reverse d) (alpha!!i) / scale
   in (c, gpf, alpha)


iirp :: ([Double], [([Double], [Double])]) -> VS.Vector Double -> VS.Vector Double
iirp (firpart, iirpart) v = case null firpart of
  True  -> applyIIR iirpart v
  False -> applyFIRIIR firpart iirpart v


{- internal functions -}
applyIIR :: [([Double], [Double])] -> VS.Vector Double -> VS.Vector Double
applyIIR coeffs v = Par.runPar $ do
  jobs <- Par.parMap (`sos1filter` v) coeffs
  return $ sum outs


applyFIRIIR :: [Double] -> [([Double], [Double])] -> VS.Vector Double -> VS.Vector Double
applyFIRIIR firpart iirpart v = unsafePerformIO $ do
  firjob' <- Par.spawnP (fir firpart v)
  firjob <- Par.get firjob'
  iirjobs<- Par.parMap (`sos1filter` v) iirpart
  return $ sum (firjob : iirjobs)


d2clist :: [Double] -> [Complex Double]
d2clist = map (:+0)



