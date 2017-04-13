{-# LANGUAGE QuasiQuotes #-}

module Main where

-- for R plot library
import Control.Monad.IO.Class (liftIO)
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import qualified H.Prelude as H
import Language.R.Instance as R
import Language.R.QQ

import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V

-- for HasKAL
import HasKAL.DetectorUtils.Detector
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.SignalProcessingUtils.Resampling
import HasKAL.WaveUtils.Data

main = R.withEmbeddedR R.defaultConfig $ do
 R.runRegion $ do
  let detector = LIGO_Hanford
      channel = "H1:LOSC-STRAIN"
      fname = "H-H1_LOSC_4_V1-1126259446-32.gwf"
  mw <- liftIO $ readFrameWaveData' detector channel fname
  case mw of
    Nothing -> error "data not found"
    Just w  -> do
      let rw = resampleWaveData (3,8) w
          dw = downsampleWaveData 1024 w
          fs = samplingFrequency w :: Double
          fsr = samplingFrequency rw :: Double
          fsd = samplingFrequency dw :: Double
          datw  = V.toList (gwdata w )
          datrw = V.toList (gwdata rw)
          datdw = V.toList (gwdata dw)
          tw  = [0,1/fs..(fromIntegral (lengthWaveData w -1))/fs]
          trw = [0,1/fsr..(fromIntegral (lengthWaveData rw -1))/fsr]
          tdw = [0,1/fsd..(fromIntegral (lengthWaveData dw -1))/fsd]
--          up = V.toList $ upsampleSV fs (fs*3) (gwdata w)
--          tu = [0,1/(fs*3)..(fromIntegral (length up -1))/(fs*3)]
--      liftIO $ print $ take 100 datrw

      [r| require("ggplot2") |]
      [r| require("scales") |]
      [r| require("plotly") |]
      [r| require("RColorBrewer") |]
      [r|
        Xv1 <- tw_hs
        Yv1 <- datw_hs
        z1 <- data.frame (Xv1,Yv1,frame=1)
        Xv2 <- trw_hs
        Yv2 <- datrw_hs
        z2 <- data.frame (Xv2,Yv2,frame=2)
        Xv3 <- tdw_hs
        Yv3 <- datdw_hs
        z3 <- data.frame (Xv3,Yv3,frame=3)
        p <- ggplot(NULL)
        p <- p + geom_line(data=z1,aes(x=Xv1,y=Yv1),color="red")
        p <- p + geom_line(data=z2,aes(x=Xv2,y=Yv2),color="blue")
        p <- p + geom_line(data=z3,aes(x=Xv3,y=Yv3),color="green")
        plabs <- p + labs(title = "GW150914", x = "time[s]", y = "strain")

        black.bold.text <- element_text(size=20,face = "bold", color = "black")
        pfin1 <- plabs + theme(title = black.bold.text
                              , axis.title = black.bold.text
                              , axis.text = black.bold.text
                              , axis.ticks.length = unit(.3, "cm")
                              , axis.ticks = element_line(size = 2))
        pplotly <- ggplotly (pfin1)
        htmlwidgets::saveWidget(as.widget(pplotly), "int_timeseriesGW150914.html")
        ggsave(file = "timeseriesGW150914.png", plot = pfin1, dpi = 100, width = 10, height = 8)
        |]
      return ();
