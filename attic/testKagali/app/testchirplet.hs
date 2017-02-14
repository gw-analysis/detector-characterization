-- #!/usr/bin/env stack
-- stack --resolver=lts-5.2 runghc

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}


import           Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector.Storable as V
import HasKAL.DetectorUtils.Detector
import HasKAL.IOUtils.Function (loadASCIIdataCV)
import HasKAL.Misc.Function (mkChunksV, mkChunksL)
import HasKAL.SignalProcessingUtils.Resampling (downsampleSV)
import HasKAL.SimulationUtils.Injection.Function (addInjsig)
import HasKAL.SpectrumUtils.SpectrumUtils(gwspectrogramWaveData)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import HasKAL.WaveUtils.Data
import qualified Numeric.LinearAlgebra as N
import qualified Numeric.GSL.Statistics as GSL

--import KAGALIUtils_new (dKGLChirpletMain)
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils as KGL

-- for plotting
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import qualified H.Prelude as H
import Language.R.Instance as R
import Language.R.QQ


main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
 R.runRegion $ do
  let v = loadASCIIdataCV "/home/kazu/attic/testKagali/app/dat/KRD_SFHx_ascii.dat"
      fs = 2048 :: Double
      fsorig = 16384 :: Double
--  let v = loadASCIIdataCV "dat/gwfSc_170r1e10_eq.fs2048.txt"
      tv' = [x | (i,x)<-zip [0..] (V.toList $ head v), i `mod` 8 == 0] :: [Double]
      htv = head tv'
      hp = downsampleSV fsorig fs $ v !! 1
      hc = downsampleSV fsorig fs $ v !! 2
      tv = [htv,htv+1/fs..(htv+(fromIntegral (V.length hp) -1)/fs)] :: [Double]
      ts' = N.scale 1E-23 $ N.randomVector 1 N.Gaussian (V.length hp) :: V.Vector Double
      ts = addInjsig 0 ts' hp
      tsll = V.toList ts
  liftIO $ print "signal-to-noise ratio is"
  liftIO $ print $ (sqrt (2 * fs)) * GSL.stddev hp / GSL.stddev ts'
  -- ref: (3.2) in https://arxiv.org/abs/gr-qc/9812015
  let tsl = mkChunksV ts 64 128
--      out = flip map tsl $ \v -> dKGLChirpletMain v fs 5 4
      out = flip map tsl $ \v -> KGL.dKGLChirpletMain v fs 5 4
      (freq,cost) = unzip out
      tv2 = concat $ mkChunksL tv 64 128
--  print "costs are"
--  print cost
      lsub = fromIntegral (V.length (head freq)) :: Double
      l = fromIntegral (length freq) :: Double
      freqL = concatMap V.toList freq ::[Double]
  let x = mkWaveData General "sim" fs (formatGPS htv) (formatGPS (htv + fromIntegral (V.length ts-1)/fs)) ts
      (sgt',sgf',sgp') = gwspectrogramWaveData 0.015 0.02 x
      sgt = V.toList sgt'
      lsgt = fromIntegral$ length sgt :: Double
      sgf = V.toList sgf'
      lsgf = fromIntegral$ length sgf :: Double
      sgp = map sqrt $ concatMap V.toList $ N.toRows sgp'

  [r| require("ggplot2") |]
  [r| require("grid")|]
  [r| require("gridExtra")|]
  [r| require("scales") |]
  [r| require("RColorBrewer") |]
  [r|
    lsub <- lsub_hs
    l <- l_hs
    freqL <- freqL_hs
    tv <- tv_hs
    tv2 <- tv2_hs
    cost <- cost_hs
    tsll <- tsll_hs
    mkfd <- function(a,b){
      xX <- a
      yY <- b
      fd <- data.frame(xX,yY)
      return(fd)
    }
    mkfd.fd <- data.frame()
    for(i in 1:l){
      f <- freqL[((i-1)*lsub+1):(i*lsub)]
      t <- tv2[((i-1)*lsub+1):(i*lsub)]
      temp <- mkfd(t,f)
      temp <- data.frame(temp, cost=cost[i], replace=T)
      mkfd.fd <- rbind(mkfd.fd, temp)
    }
    black.bold.text <- element_text(size=16,face = "bold", color = "black")
    p2 <- ggplot(mkfd.fd,aes(x=xX, y=yY, color=cost)) +
      geom_point() +
      geom_line() +
      labs(x = "time[s]", y = "frequency[Hz]") +
      theme( title = black.bold.text
           , axis.title = black.bold.text
           , axis.text = black.bold.text
           , axis.ticks.length = unit(.1, "cm")
           , axis.ticks = element_line(size = 1)) +
      xlim(0.03,0.35) + ylim(0,1000)
    scientific_10 <- function(x) {
      parse(text=gsub("e", " %*% 10^", scientific_format(digits=1)(x)))
    }
    yformatter <- function (x) {
      ind <-floor(log10(x))
      sprintf('%3.1E',x)
    }
    tsdat <- data.frame(tv,tsll)
    p1 <- ggplot(tsdat,aes(x=tv,y=tsll)) +
      geom_line() +
      labs(title = "SN_SFHx", x = "time[s]", y = "strain") +
      theme( title = black.bold.text
           , axis.title = black.bold.text
           , axis.text = black.bold.text
           , axis.ticks.length = unit(.1, "cm")
           , axis.ticks = element_line(size = 1)) +
      xlim(0.03,0.35) +
      scale_y_continuous(label=scientific_10)
    lt <- lsgt_hs
    lf <- lsgf_hs
    tR0 <- sgt_hs
    fR0 <- sgf_hs
    tR <- rep(tR0,lf)
    fR <- sort(rep(fR0,lt))
    sgpR <- sgp_hs
    df <- data.frame(T=tR,F=fR,P=log(sgpR))
    p3 <- ggplot() +
      geom_tile(data=df,aes(x=T,y=F,fill=P)) +
      scale_fill_gradient(low="white", high="green") +
      labs(x = "time[s]", y = "frequency[Hz]") +
      geom_point(data=mkfd.fd,aes(x=xX, y=yY, color=cost)) +
      scale_color_gradientn(colours = c('springgreen4', 'white')) +
      theme( title = black.bold.text
           , axis.title = black.bold.text
           , axis.text = black.bold.text
           , axis.ticks.length = unit(.1, "cm")
           , axis.ticks = element_line(size = 1)) +
      xlim(0.03,0.35) + ylim(0,1000)

    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))
    g3 <- ggplot_gtable(ggplot_build(p3))
    maxWidth <- unit.pmax(g1$widths, g2$widths, g3$widths)
    g1$widths <- maxWidth
    g2$widths <- maxWidth
    g3$widths <- maxWidth
    grid.arrange(g1, g3, ncol=1)
    g <- arrangeGrob (g1, g3, ncol=1)
    ggsave( file = "chirplet_SN_SFHx.png"
          , plot = g
          , dpi = 100
          , width = 10
          , height = 8)
    |]
  return ()
