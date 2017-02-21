{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}


module HasKAL.MonitorUtils.ChirpletMon.Plot
( plotChirpletGram
, plotChirpletGram2png
) where

-- for plotting
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import qualified H.Prelude as H
import Language.R.Instance as R
import Language.R.QQ

import qualified Data.Vector.Storable as V
import HasKAL.MonitorUtils.ChirpletMon.Function
import HasKAL.MonitorUtils.ChirpletMon.Data ( ChirpletParam(..)
                                            , ChirpletGram(..)
                                            , ChirpletPlotParam(..)
                                            )


plotChirpletGram :: ChirpletGram
                 -> ChirpletPlotParam
                 -> FilePath
                 -> IO()
plotChirpletGram cp p fname = R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
  let t = time cp
      f = frequency cp
      c = cost cp
      lseg = fromIntegral $ floor $ fromIntegral (length t) / fromIntegral (length c) :: Double
      l = fromIntegral $ length c :: Double
      (xl, xu) = xrange p
      (yl, yu) = yrange p
  [r| require("ggplot2") |]
  [r| require("grid")|]
  [r| require("gridExtra")|]
  [r| require("scales") |]
  [r|
    fname <- fname_hs
    time  <- t_hs
    fre   <- f_hs
    cost  <- c_hs
    lseg  <- lseg_hs
    l     <- l_hs
    xl    <- xl_hs
    xu    <- xu_hs
    yl    <- yl_hs
    yu    <- yu_hs
    mkfd <- function(a,b){
      xX <- a
      yY <- b
      fd <- data.frame(xX,yY)
      return(fd)
    }
    mkfd.fd <- data.frame()
    for(i in 1:l){
      f <- fre[((i-1)*lseg+1):(i*lseg)]
      t <- time[((i-1)*lseg+1):(i*lseg)]
      temp <- mkfd(t,f)
      temp <- data.frame(temp, cost=cost[i], replace=T)
      mkfd.fd <- rbind(mkfd.fd, temp)
    }
    black.bold.text <- element_text(size=16, face ="plain", color="black")
    mkfd <- function(a,b){
      xX <- a
      yY <- b
      fd <- data.frame(xX,yY)
      return(fd)
    }
    p <- ggplot(mkfd.fd,aes(x=xX, y=yY, color=cost)) +
      geom_point() +
      geom_line() +
      labs(x = "time[s]", y = "frequency[Hz]") +
      theme( title = black.bold.text
           , axis.title = black.bold.text
           , axis.text = black.bold.text
           , axis.ticks.length = unit(.1, "cm")
           , axis.ticks = element_line(size = 1)) +
      xlim(xl,xu) + ylim(yl,yu)
    ggsave( file = fname
          , plot = p
          , dpi = 100
          , width = 10
          , height = 8
          )
    |]
  return ()


plotChirpletGram2png :: ChirpletGram
                     -> ChirpletPlotParam
                     -> FilePath
                     -> IO()
plotChirpletGram2png cp p fname = R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
  let t = time cp
      f = frequency cp
      c = cost cp
      lseg = fromIntegral $ floor $ fromIntegral (length t) / fromIntegral (length c) :: Double
      l = fromIntegral $ length c :: Double
      (xl, xu) = xrange p
      (yl, yu) = yrange p
  [r| require("ggplot2") |]
  [r| require("grid")|]
  [r| require("gridExtra")|]
  [r| require("scales") |]
  [r|
    fname <- fname_hs
    time  <- t_hs
    fre   <- f_hs
    cost  <- c_hs
    lseg  <- lseg_hs
    l     <- l_hs
    xl    <- xl_hs
    xu    <- xu_hs
    yl    <- yl_hs
    yu    <- yu_hs
    mkfd <- function(a,b){
      xX <- a
      yY <- b
      fd <- data.frame(xX,yY)
      return(fd)
    }
    mkfd.fd <- data.frame()
    for(i in 1:l){
      f <- fre[((i-1)*lseg+1):(i*lseg)]
      t <- time[((i-1)*lseg+1):(i*lseg)]
      temp <- mkfd(t,f)
      temp <- data.frame(temp, cost=cost[i], replace=T)
      mkfd.fd <- rbind(mkfd.fd, temp)
    }
    black.bold.text <- element_text(size=16, face ="plain", color="black")
    mkfd <- function(a,b){
      xX <- a
      yY <- b
      fd <- data.frame(xX,yY)
      return(fd)
    }

    png(fname,width=640,height=480,units="px",bg = "transparent")
    p <- ggplot(mkfd.fd,aes(x=xX, y=yY, color=cost)) +
      geom_point() +
      geom_line() +
      labs(x = "time[s]", y = "frequency[Hz]") +
      theme( title = black.bold.text
           , axis.title = black.bold.text
           , axis.text = black.bold.text
           , axis.ticks.length = unit(.1, "cm")
           , axis.ticks = element_line(size = 1)) +
      xlim(xl,xu) + ylim(yl,yu)
    dev.off()
#      ggsave( file = fname
#            , plot = p
#            , dpi = 100
#            , width = 10
#            , height = 8
#            )
    |]
  return ()
