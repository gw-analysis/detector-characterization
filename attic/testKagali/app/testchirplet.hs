-- #!/usr/bin/env stack
-- stack --resolver=lts-5.2 runghc

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}


import           Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function (loadASCIIdataCV)
import HasKAL.Misc.Function (mkChunksV)
import HasKAL.SignalProcessingUtils.Resampling (downsampleSV)

import KAGALIUtils_new (dKGLChirpletMain)

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import qualified H.Prelude as H
import Language.R.Instance as R
import Language.R.QQ


main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
 R.runRegion $ do
  let v = loadASCIIdataCV "dat/KRD_SFHx_ascii.dat"
--  let v = loadASCIIdataCV "dat/gwfSc_170r1e10_eq.fs2048.txt"
      tv = [x | (i,x)<-zip [0..] (V.toList $ head v), i `mod` 8 == 0] :: [Double]
      hp = downsampleSV 16384 2048 $ v !! 1
      hc = downsampleSV 16384 2048 $ v !! 2
  liftIO $ print "check loading data"
  liftIO $ print $ take 5 $ tv
  let hpl = mkChunksV hp 128
      out = flip map hpl $ \v -> dKGLChirpletMain v 2048 5 4
      (freq,cost) = unzip out
--  print "costs are"
--  print cost
      lsub = fromIntegral (V.length (head freq)) :: Double
      l = fromIntegral (length freq) :: Double
      freqL = concatMap V.toList freq ::[Double]
  [r| require("ggplot2") |]
--  [r| require("scales") |]
--  [r| require("RColorBrewer") |]
  [r|
    lsub <- lsub_hs
    l <- l_hs
    freqL <- freqL_hs
    tv <- tv_hs
    cost <- cost_hs
    mkfd <- function(a,b){
      xX <- a
      yY <- b
      fd <- data.frame(xX,yY)
      return(fd)
    }
    mkfd.fd <- data.frame()
    for(i in 1:l){
      f <- freqL[((i-1)*lsub+1):(i*lsub)]
      t <- tv[((i-1)*lsub+1):(i*lsub)]
      temp <- mkfd(t,f)
      temp <- data.frame(temp, color=cost[i], replace=T)
      mkfd.fd <- rbind(mkfd.fd, temp)
    }
    black.bold.text <- element_text(size=16,face = "bold", color = "black")
    p <- ggplot(mkfd.fd,aes(x=xX, y=yY, color=color)) +
      geom_point() +
      geom_line() +
      labs(title = "SN_SFHx", x = "time[s]", y = "frequency[Hz]") +
      theme( title = black.bold.text
           , axis.title = black.bold.text
           , axis.text = black.bold.text
           , axis.ticks.length = unit(.1, "cm")
           , axis.ticks = element_line(size = 1)) +
      xlim(0.03,0.35) + ylim(0,1000)
    ggsave(file = "chirplet_SN_SFHx.png", plot = p, dpi = 100, width = 10, height = 8)
    |]
  return ()
