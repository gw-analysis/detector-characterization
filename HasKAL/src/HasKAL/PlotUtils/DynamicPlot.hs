{-# LANGUAGE ScopedTypeVariables, ParallelListComp, FlexibleContexts #-}



module HasKAL.PlotUtils.DynamicPlot where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Data.Complex
import Data.IORef
import Math.FFT
import Data.Array.CArray
import GHC.Float ( double2Float )
import Foreign.ForeignPtr
import System.Random
import Foreign (Storable)
import Foreign.Marshal.Array
import Foreign.Ptr

-- setting parameters
nfft, bufSize, rate :: Int
nfft =  1024
winSize :: Int
winSize = 2 * nfft
bufSize = winSize
rate = 44100
freqBin :: Double
freqBin = fromIntegral rate / fromIntegral winSize -- Hz
fftRange :: (Int,  Int)
fftRange = (start, end)
  where start :: Int = head $ dropWhile (cutoff $ fst $ head notetics) [1..]
        end :: Int = head $ dropWhile (cutoff $ fst $ last notetics) [1..]
        cutoff n = (<n) . hz . fromIntegral


-- frequency twiddling
setLog10 :: Double -> Double
setLog10 x = logBase 10 x
unsetLog10 :: Floating a => a -> a
unsetLog10 x = 10**x
shift :: (RealFrac a) => a -> Float
shift x = realToFrac x
hz :: Double -> Double
hz x = freqBin * x
logscale :: Double -> Float
logscale = shift . setLog10 . hz


-- defining tics
fvec :: [Double]
fvec = map (*freqBin) [1..512]
fromD2S :: [Double] -> [String]
fromD2S x = map show x
fveclabel :: [String]
fveclabel = fromD2S fvec
notetics :: [(Double, String)]
notetics = [ (x, y) | x <- [ 0 , freqBin/(realToFrac dscale) .. ] | y <- fveclabel]

fvec' :: [Double]
fvec' = map (*freqBin) [start..end]
  where start = fromIntegral $ fst fftRange :: Double
        end   = fromIntegral $ snd fftRange :: Double
fveclabel' :: [String]
fveclabel' = fromD2S fvec'
notetics' :: [(Double,  String)]
notetics' = [ (x,  y) | x <- [ 0 ,  freqBin/(realToFrac dscale) .. ] | y <- fveclabel']


-- Plot framerowk
dscale :: Float
dscale = 3
draw :: Picture -> IO Picture
draw = return . Translate (-440) (-240)
background :: Int -> Picture -> Picture
background l = Color $ makeColor8 l l l 256
grid_horizontal :: Picture
grid_horizontal = background 100 $ Pictures [ mark x y | (x, y) <- notetics' ]
  where mark x y = Pictures [ Line [ (shift x, 0), (shift x, 500) ]
                            , Translate (shift x - 5) (-5) $ Rotate 90 $ Scale 0.1 0.1 $ Text y ]
grid_vertical :: Picture
grid_vertical = background 100 $ Pictures $ concat markers
  where markers = [ [ Line [ (0, y), (shift . fst $ last notetics', y) ]
                    , Translate (-30) y $ Scale 0.1 0.1 $ Text $ show (floor y) ]
                  | y <- [50, 100 .. 500] ]


-- FFT twiddling
hamming :: (Floating a,  Integral a1,  Integral a2) => a2 -> a1 -> a -> a
hamming n i v = v * (0.53836 - 0.46164 * cos((2.0 * pi * fromIntegral i) / (fromIntegral n - 1)));

getPSD :: Floating a => a -> a -> a
--getPSD x y = sqrt $ (1 / (fromIntegral (end - start + 1) ** 2)) * (x * x + y * y)
--  where start = fst fftRange
--        end   = snd fftRange
getPSD x y = sqrt $ x * x + y * y


normalize :: a -> a
normalize = id



loop :: Ptr Double -> Ptr Double -> IORef (CArray Int Double) -> Float -> IO Picture
loop ptr_source buf win time =
    do let nptr = floor time :: Int
       let offset = nptr * bufSize :: Int
       new <- ptrToArray (ptr_source `plusPtr` offset) buf bufSize
       old <- readIORef win
       let combined = shiftArray winSize old new
       writeIORef win combined
       case bounds combined of
         (_, cwin) | cwin < winSize - 1 -> draw $ Pictures [ grid_horizontal, grid_vertical ]
                   | otherwise -> analyze combined


ptrToArray :: Storable e => Ptr e -> Ptr e -> Int -> IO (CArray Int e)
ptrToArray ptr_source buf segsize = do
    copyArray buf ptr_source segsize
    buf' <- newForeignPtr_ buf
    unsafeForeignPtrToCArray buf' (0, segsize - 1)


analyze :: (IArray a1 Double) => a1 Int Double -> IO Picture
analyze bits = draw $ Pictures
    [ grid_horizontal
    , grid_vertical
    , Color white $ plotfft bufPSD
    ]
  where bufDouble = mapArray (\i v -> hamming winSize i $ normalize v) bits
        applyFFT = array (start, end) $ drop start $ take end $ assocs $ dftRC bufDouble
        bufPSD :: CArray Int Double = liftArray (\(a :+ b) -> getPSD a b) $ applyFFT
        start = fst fftRange
        end = snd fftRange


-- array twiddling
mapArray :: (Ix t1,  IArray a e,  IArray a1 t)=>(t1 -> t -> e) -> a1 t1 t -> a t1 e
mapArray f a = array (bounds a) $ map (\(i, v) -> (i, f i v)) $ assocs a

shiftArray :: (IArray a2 e, IArray a1 e, IArray a e)=>Int -> a1 Int e -> a2 Int e -> a Int e
shiftArray datasize old new = array (0, datasize' - 1) $ (zip [0..] $ drop remove $ elems old) ++ zip [retain..] (elems new)
  where remove = datadim old - retain
        retain = minimum [ datasize - datadim new, datadim old ]
        datasize' = retain + datadim new
        datadim x = 1 + snd (bounds x) - fst (bounds x)

plotfft :: (Integral a1, Ix a1, IArray a Double) => a a1 Double -> Picture
plotfft bits = Line $ [ (double2Float freqBin / dscale * fromIntegral x, double2Float $ y) | (x, y) <- assocs bits ]



main :: IO ()
main = do
  let sourcelist = take 441000 $ randomRs (2, 20) $ mkStdGen 1 :: [Double]
  withArray sourcelist $ \ptrs -> do
    allocaArray bufSize $ \buf -> do
      win <- newIORef $ array (0, 0) []
      animateIO (InWindow "Spectrum" (1024,600) (0,0)) black (loop ptrs buf win)











