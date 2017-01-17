{-# LANGUAGE QuasiQuotes #-}

module Main where

--import LibHasKAL
import Data.Maybe (fromJust)
import qualified H.Prelude as H
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.DetectorUtils.Detector(Detector(..))
import HasKAL.WaveUtils.Data(WaveData(..))
import qualified Data.Vector.Storable as V
import HasKAL.TimeUtils.Signature
import HasKAL.TimeUtils.Function
import HasKAL.WaveUtils.Signature
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
  R.runRegion $ do
    let fname = "/home/kazu/work/data/gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"
    let chname = "H1:LOSC-STRAIN"
    maybewave <- liftIO $ readFrameWaveData' General chname fname
    let x = fromJust maybewave
    let y = V.toList $ gwdata x
--    print $ take 5 y
    let ylen = length y
        t0 = deformatGPS $ startGPSTime x
        fs = samplingFrequency x
        dat = zip [t0,t0+1/fs..] y
        tv = take (length y) [0,1/fs..] :: [Double]
    [r| require("ggplot2") |]
    [r| require("scales") |]
    [r| require("plotly") |]
    [r|
      Xv <- tv_hs
      Yv <- y_hs
      z <- data.frame (Xv,Yv)
      p <- ggplot(z, aes(x = Xv, y = Yv)) + geom_line(color="red")
      plabs <- p + labs(title = "GW150914", x = "time[s]", y = "strain")
      black.bold.text <- element_text(size=20,face = "bold", color = "black")
      pfin1 <- plabs + theme(title = black.bold.text
                            , axis.title = black.bold.text
                            , axis.text = black.bold.text
                            , axis.ticks.length = unit(.3, "cm")
                            , axis.ticks = element_line(size = 2))
      pfin2 <- pfin1 + coord_cartesian(xlim = c(10,20), ylim = NULL)
      yformatter <- function (x) {
        ind <-floor(log10(x))
        sprintf('%3.1E',x)
      }
      scientific_10 <- function(x) {
        parse(text=gsub("e", " %*% 10^", scientific_format(digits=1)(x)))
      }
      pfin <- pfin2 + scale_y_continuous(label=scientific_10)
      pplotly <- ggplotly (pfin)
      htmlwidgets::saveWidget(as.widget(pplotly), "graph.html")
      ggsave(file = "gw150914.png", plot = pfin, dpi = 100, width = 10, height = 8)
      |]
    return ()
