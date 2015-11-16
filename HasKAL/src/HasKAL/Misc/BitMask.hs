

module HasKAL.Misc.BitMask (
    getBits
  , getBitsV
  , intToBin
  , readBin
  , showBin
  ) where

import Data.Char (digitToInt)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V (map)

getBits :: (Integral a) 
        => [a] -- ^ List of decimal number
        -> Int -- ^ Index from LSB (0,1,2...)
        -> [a] -- ^ List of n-th bits
getBits xs n = map (getBit n) xs

getBitsV :: Vector Int -- ^ Vector of decimal number
         -> Int        -- ^ Index from LSB (0,1,2...)
         -> Vector Int -- ^ Vector of n-th bits
getBitsV xs n = V.map (getBit n) xs

getBit :: (Integral a)
       => Int -- ^ Index from LSB
       -> a   -- ^ Decimal number
       -> a   -- ^ n-th bit
getBit n x
  | n > (len-1) = 0
  | otherwise   = y !! (len-n-1)
  where y = intToBin x
        len = length y

-- Convert
intToBin :: (Integral a) 
         => a   -- ^ Decimal number
         -> [a] -- ^ List of bits (head=MSB, last=LSB)
intToBin 0 = [0]
intToBin n
  | n < 0     = error ""
  | otherwise = intToBinCore n []
  where intToBinCore 0 s = s
        intToBinCore n s = intToBinCore (n`div`2) $ (n`mod`2) : s


-- Read, Show
readBin :: (Eq a, Num a) => ReadS a
readBin ('0':'0':ss) = readBin ('0':ss)
readBin ss
  | len == 0  = []
  | otherwise = [(fromIntegral $ readBinCore nss, sss)]
  where nss = takeWhile (\x -> or [(x=='0'), (x=='1')]) ss :: String
        len = length nss
        sss = drop len ss :: String
        readBinCore (n:[]) = (digitToInt n)
        readBinCore (n:ns) = (digitToInt n) * 2^(length ns) + readBinCore ns :: Int

showBin :: (Integral a) => a -> ShowS
showBin 0 s = '0' : s
showBin n s
  | n < 0     = error ""
  | otherwise = showBinCore n s
  where showBinCore 0 s = s
        showBinCore n s = showBinCore (n`div`2) $ i2c (n`mod`2) : s
        i2c = ("01" !!) . fromIntegral

