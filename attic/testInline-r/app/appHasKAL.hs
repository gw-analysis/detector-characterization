
{-# LANGUAGE QuasiQuotes #-}


module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import qualified H.Prelude as H
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.DetectorUtils.Detector(Detector(..))
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV,whiteningWaveData')
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV,gwOnesidedPSDWaveData,gwspectrogramWaveData)
import HasKAL.TimeUtils.Signature
import HasKAL.TimeUtils.Function
import HasKAL.WaveUtils.Data(WaveData(..))
import HasKAL.WaveUtils.Signature
import Language.R.Instance as R
import Language.R.QQ
import qualified Numeric.LinearAlgebra as NL


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
    [r| require("RColorBrewer") |]
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
      pfina <- pfin2 + scale_y_continuous(label=yformatter)
      pplotly <- ggplotly (pfina)
      htmlwidgets::saveWidget(as.widget(pplotly), "int_timeseriesGW150914.html")
      ggsave(file = "timeseriesGW150914.png", plot = pfin, dpi = 100, width = 10, height = 8)
      |]
    let p = 1500
        nfft = floor fs
--        spec = gwpsdV (gwdata x) nfft fs
        whnspe = gwOnesidedPSDWaveData 0.2 x
        freV = V.toList . fst $ whnspe
        speV = V.toList . snd $ whnspe
    [r|
      Xv <- freV_hs
      Yv <- speV_hs
      z <- data.frame (Xv,Yv)
      p <- ggplot(z, aes(x = Xv, y = Yv)) + geom_line(color="red")
      p <- p + labs(title = "GW150914 data", x = "frequency[Hz]", y = "ASD[1/Hz]")
#      xbreaks <- c(10,20,40,60,100,200,400,1000,2000)
#      ybreaks <- seq(0,0.025,0.005)
      p <- p + scale_x_log10()
      p <- p + scale_y_log10()
      black.bold.text <- element_text(size=16,face = "bold", color = "black")
      p <- p + theme( title = black.bold.text
                    , axis.title = black.bold.text
                    , axis.text = black.bold.text
                    , axis.ticks.length = unit(.1, "cm")
                    , axis.ticks = element_line(size = 1))
      ggsave(file = "spectrumGW150914.png", plot = p, dpi = 100, width = 10, height = 8)
      |]
    [r|require("fields") |]
    let (sgt',sgf',sgp') = gwspectrogramWaveData 0.1 0.2 x
        sgt = V.toList sgt'
        lsgt = fromIntegral$ length sgt :: Double
        sgf = V.toList sgf'
        lsgf = fromIntegral$ length sgf :: Double
        sgp = map sqrt $ concatMap V.toList $ NL.toRows sgp'
        sgpC = map sqrt $ concatMap V.toList $ NL.toColumns sgp'
    [r|
      lt <- lsgt_hs
      lf <- lsgf_hs
      tR0 <- sgt_hs
      fR0 <- sgf_hs
      tR <- rep(tR0,lf)
      fR <- sort(rep(fR0,lt))
      sgpR <- sgp_hs
      df <- data.frame(T=tR,F=fR,P=log(sgpR))
      p<-ggplot(df,aes(x=T,y=F,fill=P))+geom_tile()
      p<-p+scale_fill_gradient(low="white", high="orange")
      p <- p + labs(title = "GW150914 data", x = "time[s]", y = "frequency[Hz]")
      pp <-ggplotly(p)
      htmlwidgets::saveWidget(as.widget(pp), "int_specgramGW150914.html")
      ggsave(file = "specgramW150914.png", plot = p, dpi = 100, width = 10, height = 8)
      |]
--    [r|
--            l <- lsgt_hs
--      #      tR <- sgt_hs
--      #      fR <- sgf_hs
--      #      sgpR <- sgp_hs
--      #      pR <- matrix(sgpR,l)
--      #      # pRf <- as.data.frame(pRm)
--      #      par(mar = c(5.5, 6.0, 4.1, 2))
--      #      png("specgramGW150916.png")
--      #      image.plot(tR,fR,log(pR),xlab="time[s]",ylab="frequency[Hz]",font.lab=2,font.axis=2,cex.lab=2,cex.axis = 1.5)
--      #      dev.off()
--      #      |]

    [r|
      l <- lsgt_hs
      lf <- lsgf_hs
      tR <- sgt_hs
      fR <- sgf_hs
      sgpR <- sgpC_hs
      mkspec <- function(a,b){
        xX <- a
        yY <- b
        spec <- data.frame(xX,yY)
        return(spec)
      }
      mkspec.spec <- data.frame()
      for(i in 1:l){
        tempP <- sgpR[((i-1)*lf+1):(i*lf)]
        temp <- mkspec(fR,tempP)
        temp <- data.frame(temp, frame=tR[i], replace=T)
        mkspec.spec <- rbind(mkspec.spec, temp)
      }
      p <- plot_ly(mkspec.spec,x=~xX, y=~yY, frame=~frame) %>%
        layout(xaxis=list(type="log")) %>%
        layout(yaxis=list(type="log")) %>%
        add_trace(type='scatter',mode = 'lines')
#      + geom_line(color="red")
#      p <- p + labs(title = "GW150914 data", x = "frequency[Hz]", y = "ASD[1/Hz]")
#      xbreaks <- c(10,20,40,60,100,200,400,1000,2000)
#      ybreaks <- seq(0,0.025,0.005)
#      p <- p + scale_x_log10(breaks=xbreaks,labels=xbreaks)
#      p <- p + scale_y_log10(breaks=ybreaks,labels=ybreaks)
#      black.bold.text <- element_text(size=16,face = "bold", color = "black")
#      p <- p + theme( title = black.bold.text
#                    , axis.title = black.bold.text
#                    , axis.text = black.bold.text
#                    , axis.ticks.length = unit(.1, "cm")
#                    , axis.ticks = element_line(size = 1))
#      pp <- ggplotly(p)
      htmlwidgets::saveWidget(as.widget(p), "int_spectrumGW150914.html")
      |]

    return ()
