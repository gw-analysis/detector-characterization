{- Infinit and Finite Inpulse Response Functions
- modified versiobn of dsp package : http://hackage.haskell.org/package/dsp -}


module HasKAL.SignalProcessingUtils.FilterH
( biquad_df1
, biquad_df2
, biquad_df2t
, iir_df1
, iir_df2
, fir
) where

import Data.Array.Unboxed


-- | Direct Form I for a second order section
--
--  @v[n] = b0 * x[n] + b1 * x[n-1] + b2 * x[n-2]@
--
--  @y[n] = v[n] - a1 * y[n-1] - a2 * y[n-2]@

biquad_df1 :: Num a => a -- ^ a_1
    -> a -- ^ a_2
    -> a -- ^ b_0
    -> a -- ^ b_1
    -> a -- ^ b_2
    -> [a] -- ^ x[n]
    -> [a] -- ^ y[n]
biquad_df1 a1 a2 b0 b1 b2 x = df1 a1 a2 b0 b1 b2 0 0 0 0 x

df1 :: Num a => a -> a -> a -> a -> a -> a -> a -> a -> a -> [a] -> [a]
df1 _  _  _  _  _  _  _  _  _  []     = []
df1 a1 a2 b0 b1 b2 x1 x2 y1 y2 (x:xs) = y : df1 a1 a2 b0 b1 b2 x x1 y y1 xs
    where v = b0 * x + b1 * x1 + b2 * x2
          y = v - a1 * y1 - a2 * y2


-- | Direct Form II for a second order section (biquad)
--
--  @w[n] = -a1 * w[n-1] - a2 * w[n-2] + x[n]@
--
--  @y[n] = b0 * w[n] + b1 * w[n-1] + b2 * w[n-2]@

biquad_df2 :: Num a => a -- ^ a_1
    -> a -- ^ a_2
    -> a -- ^ b_0
    -> a -- ^ b_1
    -> a -- ^ b_2
    -> [a] -- ^ x[n]
    -> [a] -- ^ y[n]
biquad_df2 a1 a2 b0 b1 b2 x = df2 a1 a2 b0 b1 b2 0 0 x

df2 :: Num a => a -> a -> a -> a -> a -> a -> a -> [a] -> [a]
df2 _  _  _  _  _  _  _  []     = []
df2 a1 a2 b0 b1 b2 w1 w2 (x:xs) = y : df2 a1 a2 b0 b1 b2 w w1 xs
    where w = x - a1 * w1 - a2 * w2
          y = b0 * w + b1 * w1 + b2 * w2


-- | Transposed Direct Form II for a second order section
--
--  @v0[n] = b0 * x[n] + v1[n-1]@
--
--  @y[n] = v0[n]@
--
--  @v1[n] = -a1 * y[n] + b1 * x[n] + v2[n-1]@
--
--  @v2[n] = -a2 * y[n] + b2 * x[n]@

biquad_df2t :: Num a => a -- ^ a_1
     -> a -- ^ a_2
     -> a -- ^ b_0
     -> a -- ^ b_1
     -> a -- ^ b_2
     -> [a] -- ^ x[n]
     -> [a] -- ^ y[n]
biquad_df2t a1 a2 b0 b1 b2 x = df2t a1 a2 b0 b1 b2 0 0 x

df2t :: Num a => a -> a -> a -> a -> a -> a -> a -> [a] -> [a]
df2t _  _  _  _  _  _   _   []     = []
df2t a1 a2 b0 b1 b2 v11 v21 (x:xs) = y : df2t a1 a2 b0 b1 b2 v1 v2 xs
    where v0 = b0 * x + v11
          y = v0
          v1 = -a1 * y + b1 * x + v21
          v2 = -a2 * y + b2 * x


-- | Direct Form I IIR
--
-- @v[n] = sum(k=0..M) b_k*x[n-k]@
--
-- @y[n] = v[n] - sum(k=1..N) a_k*y[n-k]@
--
-- @v[n]@ is calculated with 'fir'

iir_df1 :: (Num a, Eq a) => ([a], [a]) -- ^ (b, a)
  -> [a] -- ^ x[n]
  -> [a] -- ^ y[n]
iir_df1 (b, a) x = y
    where y = iir_df1' (b', a') x
          b' = listArray (0, (length b)-1) b
          a' = listArray (0, (length a)-1) a

iir_df1' :: (Num a, Eq a) => (Array Int a, Array Int a) -- ^ (b,a)
  -> [a] -- ^ x[n]
  -> [a] -- ^ y[n]
iir_df1' (b,a) x = y
    where v = fir (elems b) x
          y = iir'df1 a w v
          w = listArray (1,n) $ repeat 0
          n = snd $ bounds a

iir'df1 :: (Num a) => Array Int a -> Array Int a -> [a] -> [a]
iir'df1 _ _ []  = []
iir'df1 a w (v:vs) = y : iir'df1 a w' vs
    where y  = v - sum [ a!i * w!i | i <- [1..n] ]
          w' = listArray (1,n) $ y : elems w
          n  = snd $ bounds a


-- | Direct Form II IIR
--
-- @w[n] = x[n] - sum(k=1..N) a_k*w[n-k]@
--
-- @y[n] = sum(k=0..M) b_k*w[n-k]@

iir_df2 :: (Num a, Eq a) => ([a], [a]) -- ^ (b, a)
  -> [a] -- ^ x[n]
  -> [a] -- ^ y[n]
iir_df2 (b, a) x = y
    where y = iir_df2' (b', a') x
          b' = listArray (0, (length b)-1) b
          a' = listArray (0, (length a)-1) a

iir_df2' :: (Num a) => (Array Int a, Array Int a) -- ^ (b,a)
  -> [a] -- ^ x[n]
  -> [a] -- ^ y[n]
iir_df2' (b,a) x = y
    where y = iir'df2 (b,a) w x
          w = listArray (0,mn) $ repeat 0
          m = snd $ bounds b
          n = snd $ bounds a
          mn = max m n

iir'df2 :: (Num a) => (Array Int a,Array Int a) -> Array Int a -> [a] -> [a]
iir'df2 _     _ []     = []
iir'df2 (b,a) w (x:xs) = y : iir'df2 (b,a) w' xs
    where y  = sum [ b!i * w'!i | i <- [0..m] ]
          w0 = x - sum [ a!i * w'!i | i <- [1..m] ]
          w' = listArray (0,mn) $ w0 : elems w
          m  = snd $ bounds b
          mn = snd $ bounds w


--
-- @y[n] = sum(k=0,M) h[k]*x[n-k]@
--
fir :: (Num a, Eq a) => [a] -- ^ h[n]
   -> [a] -- ^ x[n]
   -> [a] -- ^ y[n]
fir h x = y
    where y = fir' h' x
          h' = listArray (0, (length h)-1) h

fir' :: (Num a, Eq a) => Array Int a -- ^ h[n]
    -> [a] -- ^ x[n]
    -> [a] -- ^ y[n]
fir' _ [] = []
fir' h (x:xs) | isFIRType1 h = fir'1 h w xs
             | isFIRType2 h = fir'2 h w xs
             | isFIRType3 h = fir'3 h w xs
             | isFIRType4 h = fir'4 h w xs
             | otherwise    = fir'0 h w xs
    where w = listArray (0,m) $ x : replicate m 0
          m = snd $ bounds h


-- Asymmetric FIR
fir'0 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'0 h w []     = y : []
    where y  = sum [ h!i * w!i | i <- [0..m] ]
          m  = snd $ bounds h
fir'0 h w (x:xs) = y : fir'0 h w' xs
    where y  = sum [ h!i * w!i | i <- [0..m] ]
          w' = listArray (0,m) $ x : elems w
          m  = snd $ bounds h

-- Type 1: symmetric FIR, even order / odd length
fir'1 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'1 h w []     = y : []
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2-1] ]
          m  = snd $ bounds h
          m2 = m `div` 2
fir'1 h w (x:xs) = y : fir'1 h w' xs
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2-1] ]
          w' = listArray (0,m) $ x : elems w
          m  = snd $ bounds h
          m2 = m `div` 2

-- Type 2: symmetric FIR, odd order / even length
fir'2 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'2 h w []     = y : []
    where y  = sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2] ]
          m  = snd $ bounds h
          m2 = m `div` 2
fir'2 h w (x:xs) = y : fir'2 h w' xs
    where y  = sum [ h!i * (w!i + w!(m-i)) | i <- [0..m2] ]
          w' = listArray (0,m) $ x : elems w
          m  = snd $ bounds h
          m2 = m `div` 2

-- Type 3: anti-symmetric FIR, even order / odd length
fir'3 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'3 h w []     = y : []
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2-1] ]
          m  = snd $ bounds h
          m2 = m `div` 2
fir'3 h w (x:xs) = y : fir'3 h w' xs
    where y  = h!m2 * w!m2 + sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2-1] ]
          w' = listArray (0,m) $ x : elems w
          m  = snd $ bounds h
          m2 = m `div` 2

-- Type 4: anti-symmetric FIR, off order / even length
fir'4 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
fir'4 h w []     = y : []
    where y  = sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2] ]
          m  = snd $ bounds h
          m2 = m `div` 2
fir'4 h w (x:xs) = y : fir'4 h w' xs
    where y  = sum [ h!i * (w!i - w!(m-i)) | i <- [0..m2] ]
          w' = listArray (0,m) $ x : elems w
          m  = snd $ bounds h
          m2 = m `div` 2


isFIRType1 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType1 h = even m && (h' == (reverse h'))
    where m = snd $ bounds h
          h' = elems h

isFIRType2 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType2 h = odd m && (h' == (reverse h'))
    where m = snd $ bounds h
          h' = elems h

isFIRType3 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType3 h = even m && ha == reverse hb
    where m = snd $ bounds h
          h' = elems h
          ha = take n h'
          hb = map negate (drop (n+1) h')
          n = m `div` 2

isFIRType4 :: (Num a, Eq a) => Array Int a -> Bool
isFIRType4 h = odd m && ha == reverse hb
    where m = snd $ bounds h
          ha = elems h
          hb = fmap negate $ ha




