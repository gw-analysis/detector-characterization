

module HasKAL.SignalPocessingUtils.StateSpace
( tf2NthStateSpace
, barnes
, sosstatespace
, sos1statespaceInit 
) where


import qualified Data.Vector.Storable as VS (Vector, length, unsafeWith, unsafeFromForeignPtr0,map)
import Data.List (unzip4)
import Data.Word
import Foreign.C.Types
-- import Foreign.C.String
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Array
import Foreign.Ptr
import HasKAL.SignalProcessingUtils.Parallel
import HasKAL.SignalProcessingUtils.Filter (calcInitCond)
import Numeric.LinearAlgebra
import System.IO.Unsafe


{- exposed functions -}

tf2NthStateSpace :: ([Double], [Double])
                 -> (Matrix Double, Matrix Double, Matrix Double, Matrix Double)
tf2NthStateSpace (num, denom) =
  let lendenom = length denom - 1
      lennum   = length num   - 1
      maxn     = max lendenom lennum
      a | maxn > lendenom = denom ++ replicate (maxn-lendenom) (0::Double)
        | otherwise = denom
      b | maxn > lennum = num ++ replicate (maxn-lennum) (0::Double)
        | otherwise = num
      xx = toRows . fromColumns
        $ toColumns (ident (maxn-1)::Matrix Double)
        ++ [fromList (replicate (maxn-1) (0::Double))]
      aa = fromRows $ fromList (map (*(-1)) (tail a)) : xx
      bb = (maxn >< 1) ((1::Double) : replicate (maxn-1) (0::Double))
      cc = (1 >< maxn) $ zipWith (\x y -> x - head b * y) (tail b) (tail a)
      dd = (1 >< 1) [head b]
   in (aa, bb, cc, dd)


barnes :: ([Double], [Double])
       -> [(Matrix Double, Matrix Double, Matrix Double, Double)]
barnes (num, denom) =
  let (x, y, z) = tf2cparallel (num, denom)
      (a, b) = tf2rparallel (num, denom) 
      d = map head $ fst $ unzip b
      e = map calcSS [(b, ar:+ai) | (b, ar:+ai) <- (zip y z), ai>0]
   in [(e1,e2,e3,d1)| (e1,e2,e3)<-e,d1<-d]


sosstatespace :: ([Double], [Double]) -> VS.Vector Double -> VS.Vector Double
sosstatespace coeff inputV = 
 let ss = barnes coeff
     ilen = VS.length inputV
     inputV' = d2cdV inputV :: VS.Vector CDouble
     (a',b',c',d') = unzip4 ss
     a = map (\i->d2cd . concat . toLists $ (a'!!i)) [0..length a']
     b = map (\i->d2cd . concat . toLists $ (b'!!i)) [0..length b']
     c = map (\i->d2cd . concat . toLists $ (c'!!i)) [0..length c']
     d = d2cd d'
     (_, soscoeffs) = tf2rparallel coeff
     initcoeff = map (\i -> calcInitCond (soscoeffs!!i)) [0..length soscoeffs-1]
     x01 = d2cd $ map (\x-> head x) initcoeff
     x02 = d2cd $ map (\x -> head . drop 1 $ x) initcoeff
     go iV 0  = iV  
     go iV k  = let newiV = sosstatespaceCore iV ilen (a!!k) (b!!k) (c!!k) (d!!k) (x01!!k) (x02!!k)
                 in go newiV (k-1)
  in cd2dV $ go inputV' (length a -1)


sos1statespaceInit :: ([Double], [Double]) -> (Double, Double) -> VS.Vector Double -> VS.Vector Double
sos1statespaceInit coeff initcoeff inputV = 
 let ss = barnes coeff
     ilen = VS.length inputV
     inputV' = d2cdV inputV :: VS.Vector CDouble
     (a',b',c',d') = unzip4 ss
     a = map (\i->d2cd . concat . toLists $ (a'!!i)) [0..length a']
     b = map (\i->d2cd . concat . toLists $ (b'!!i)) [0..length b']
     c = map (\i->d2cd . concat . toLists $ (c'!!i)) [0..length c']
     d = d2cd d'
     (_, soscoeffs) = tf2rparallel coeff
     x01 = [realToFrac . fst $ initcoeff]
     x02 = [realToFrac . snd $ initcoeff]
     go iV 0  = iV  
     go iV k  = let newiV = sosstatespaceCore iV ilen (a!!k) (b!!k) (c!!k) (d!!k) (x01!!k) (x02!!k)
                 in go newiV (k-1)
  in cd2dV $ go inputV' (length a -1)


{- internal functions -}

sosstatespaceCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> CDouble -> CDouble -> CDouble -> VS.Vector CDouble
sosstatespaceCore input ilen a b c d x01 x02 
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray a $ \ptrA ->
   withArray b $ \ptrB ->
   withArray c $ \ptrC ->
   allocaArray ilen $ \ptrOutput ->
   do c'sosstatespace ptrInput wilen ptrA ptrB ptrC d x01 x02 ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen


calcSS :: (Complex Double, Complex Double)
       -> (Matrix Double,  Matrix Double,  Matrix Double)
calcSS (num, denom) =
  let alphar :+ alphai = num
      sigma :+ omega  = denom
      p = realPart (abs num) / (1.0 - realPart (abs ((sigma :+ omega)*(sigma :+ (-omega)))))
      r :+ q = num /((1:+0) - abs ((sigma :+ omega)*(sigma :+ (-omega))))
      kappa = ((q + q)/(p - q))**0.5
      ao = (2 >< 2) [sigma, kappa*omega, -omega/kappa, sigma]
      bo = (2 >< 1) [((realPart (abs num) - alphai)/(p-q))**0.5
                    , (-(realPart (abs num) + alphai)/(p+q))**0.5*sign alphar]
      co = (1 >< 2) [alphar/bo @@>(0, 0), alphar/bo @@>(1, 0)]
   in (ao, bo, co)


sign x
  | x < 0 = -1
  | x >= 0 = 1
  | otherwise = error "x should be minus or plus"


itow32 :: Int -> CUInt
itow32 = fromIntegral

d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac

d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac

cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac

fst' (a, _, _, _) = a
snd' (_, b, _, _) = b
thd' (_, _, c, _) = c
frh' (_, _, _, d) = d

foreign import ccall "filterFunction.h sosstatespace" c'sosstatespace :: Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO ()


