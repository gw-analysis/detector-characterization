#!/usr/bin/env stack
-- stack --resolver=lts-5.2 runghc

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function (loadASCIIdataCV)
import HasKAL.Misc.Function (mkChunksV)
import HasKAL.SignalProcessingUtils.Resampling (downsampleSV)

import KAGALIUtils_new (dKGLChirpletMain)

main :: IO ()
main = do
  let v = loadASCIIdataCV "dat/KRD_SFHx_ascii.dat"
      tv = [x | (i,x)<-zip [0..] (V.toList $ head v), i `mod` 8 == 0]
      hp = downsampleSV 16384 2048 $ v !! 1
      hc = downsampleSV 16384 2048 $ v !! 2
  print "check loading data"
  print $ take 5 $ tv
  let hpl = mkChunksV hp 128
      out = flip map hpl $ \v -> dKGLChirpletMain v 2048 5 5
      (freq,cost) = unzip out
  print "costs are"
  print cost

-- dKGLChirpletMain :: VS.Vector Double    -- ^ Input Vector (frame)
--                  -> Double              -- ^ Sampling frequency (fs)
--                  -> Double              -- ^ maxLength (alpha)
--                  -> Int                 -- ^ # of paths used (ipath)
--                  -> ( VS.Vector Double  -- ^ Output Vector (freq)
--                     , Double)           -- ^ Output value (cost)

