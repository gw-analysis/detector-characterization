{-****************************************************
 *Mblt.hs
 *Created:2014/08/26
 *Author:Mitsuhiro Asano
 ****************************************************
Last Modified: 2014/10/06 16:37:42
-}

module HasKAL.LineUtils.LineRemoval.Mblt(
mblt
)
where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad
import System.IO 
import Data.List

-- Filter --
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType

-- module --
--import HasKAL.LineUtils.LineRemoval.RngMedian as RM
import qualified RngMedian as RM
import HasKAL.SpectrumUtils.SpectrumUtils as HSU

-- Function --
mblt :: [Double] -> Double -> [(Double,Double)] -> Int -> [Double]
mblt input fs finfo niter = do 
     let tser = input						:: [Double]
     	 dt = 1/fs						:: Double
       	 lentser = length tser					:: Int
         fcList = map fst finfo					:: [Double] 
	 bandList = map snd finfo 				:: [Double]
	 nlines = length fcList					:: Int
	 zLine = replicate nlines tser   	   		:: [[Double]]
     afterMblt <- forM [iNiter | iNiter <- [0..niter-1]] $ \iNiter -> do     --afterMblt :: [[Double]]
     	       yLine <- forM [iLines | iLines <- [0..nlines-1]] $ \iLines -> do        	--yLine :: [[Double]]
     	       	     let zSub = zLine !! iLines						:: [Double]
     	       	     	 band = bandList !! iLines      				:: Double
	       		 nblocks = truncate (1.0 / band) 	 			:: Int
 	       		 fc = fcList !! iLines 						:: Double
	       		 demodRe = map fst (demodulate zSub fs fc lentser) 		:: [Double]
	       		 demodIm = map snd (demodulate zSub fs fc lentser) 		:: [Double]
	       		 medRe = RM.rngMed demodRe lentser nblocks 			:: [Double]
	       		 medIm = RM.rngMed demodIm lentser nblocks 			:: [Double]
 	      		 modData = modulate medRe medIm fs fc lentser  			:: [Double]
	             return modData    
		     --nlines loop closed		     
               let sumy = sumListList yLine						:: [Double]
     	       zLine <- forM [iLines | iLines <- [0..nlines-1]] $ \iLines -> do  	--zLine :: [[Double]]
     	       	     let zLineSub = zipWith (+) (yLine !! iLines) (zipWith (-) tser sumy)
	   	     return zLineSub
               let output = zipWith (-) tser sumy					:: [Double]
	       return output
	       --niter loop closed
     last afterMblt

--Internal Function--
demodulate :: [Double] -> Double -> Double -> Int -> [(Double,Double)]
demodulate input fs fc lentser = do 
	   let dt = 1/fs
	       t = take lentser [0,dt..]
	       trig = map (*trigInner) t  
	       trigInner = 2 * pi * fc
	       cosT = map cos trig	      
	       sinT = map sin trig
	       x1 = map (*2) $ productList input cosT
	       x2 = map (*2) $ productList input sinT
	       -- Filter --
	       n = 2
	       (numCoeff,denomCoeff) = butter n fs fc Low
	       demodulateRe = iirFilter x1 numCoeff denomCoeff 
	       demodulateIm = iirFilter x2 numCoeff denomCoeff 
	   zip demodulateRe demodulateIm

modulate :: [Double] -> [Double] -> Double -> Double -> Int -> [Double]
modulate inputRe inputIm fs fc lentser = do
	 let dt = 1/fs
	     t = take lentser [0,dt..]
	     cosT = map cos trig
	     trig = map (*trigInner) t  
	     trigInner = 2 * pi * fc
	     sinT = map sin trig
	     modulateData = zipWith (+) (productList inputRe cosT) (productList inputIm sinT) 
	 modulateData
        
productList :: [Double] -> [Double] -> [Double]
productList xlist ylist = zipWith (*) xlist ylist

sumListList :: [[Double]] -> [Double]
sumListList input = do
	    let inputTrans = transpose input
	    	suminput = map sum inputTrans
	    suminput	

powList :: [Double] -> [Double]
powList input = zipWith (*) input input
