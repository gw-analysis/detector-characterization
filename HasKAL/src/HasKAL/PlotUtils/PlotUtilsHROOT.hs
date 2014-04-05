{-# LANGUAGE ScopedTypeVariables #-}

module PlotUtilsHROOT
       ( plot_spectrum ) where

import HasKAL.FrameUtils.FrameUtils

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

import HROOT hiding (eval)

plot_spectrum::String -> IO()
plot_spectrum dataName = do
      	       fdata <- readFrame "Channel_Name" dataName
	       let sampleRate = 1000::Double
	       let dat = subVector 1 15000 (fromList $ map realToFrac (eval fdata))
	           fft_val = fft $ dat
		   power =  map abs $ toList $ fst . fromComplex $ fft_val * conj fft_val
                   len_power = length power
                   scale_psd = 1/((fromIntegral len_power) * sampleRate)
		   len_power2 = floor $ fromIntegral(len_power)/2
		   powerspectrum = take len_power2  $ map (*scale_psd) power

	       let sampleRate2 = sampleRate/2
	           lstx = map (sampleRate2 *) $ toList (linspace len_power2 (0, 1::Double))
		   lsty = powerspectrum
		   n = length lsty

	       tapp <- newTApplication "1" [0] ["2"]
	       tcanvas <- newTCanvas "Graph1" "Graph2" 640 480
	       g1 <- newTGraph n lstx lsty

	       draw g1 "AC"
	       run tapp 1
	       delete g1
	       delete tapp



