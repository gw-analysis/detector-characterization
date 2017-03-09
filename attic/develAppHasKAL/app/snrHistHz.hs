


-- #!/usr/bin/env stack --resolver=lts-5.2 --extra-lib-dirs=/home/detchar/tools/lib --extra-include-dirs=/home/detchar/tools/include runghc --package=HasKAL


{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}


import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import qualified H.Prelude as H
import Language.R.Instance as R
import Language.R.QQ

import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as M

import HasKAL.MonitorUtils.GlitchMon.DBFunction (extractTrigInfoTFSNRSize)
import HasKAL.StatisticsUtils.Function (histogram1d)
import HasKAL.TimeUtils.GPSfunction

import System.IO (hFlush, stdout)

main = R.withEmbeddedR R.defaultConfig $ do
 R.runRegion $ do
  let gpsstart = read (time2gps "2016-04-11 09:00:00 JST") :: Int
      gpsstop =  read (time2gps "2016-04-25 17:00:00 JST") :: Int
      snrlow = 1
      snrhigh = 10000
      flow = 0
      fhigh = 2000
      fl = [0,100..1900]
      fu = (drop 1 fl) ++ [2000]
      fband = zip fl fu

  maybeout <- liftIO $
    mapM (\x-> extractTrigInfoTFSNRSize gpsstart gpsstop snrlow snrhigh (fst x) (snd x)) fband

  let snr = flip map maybeout $ \x-> case x of
        Nothing -> []
        Just out -> [snr' | (t,f,snr',nsize)<-out]
      binInterval = (logBase 10 snrhigh - logBase 10 snrlow)/fromIntegral 100
      binlist = map (10**) [logBase 10 snrlow, logBase 10 snrlow+binInterval ..logBase 10 snrhigh]
      histout = unzip $ map (histogram1d snrlow snrhigh binlist) snr
      snrmap = concatMap V.toList $ M.toRows $ M.fromColumns $ map V.fromList $ snd histout
      snrlist = head $ fst histout
      lbin = fromIntegral (length fu) :: Double
      lsnr = fromIntegral (length snrlist) :: Double
  liftIO $ print (length snr)
  liftIO $ print (length (snr!!0))
  liftIO $ print $ take 10 (snr!!0)
  liftIO $ print lbin
  liftIO $ print lsnr
  liftIO $ print (maximum snrmap)
  liftIO $ print (nimimum snrmap)

  [r| require("ggplot2") |]
  [r| require("grid")|]
  [r| require("gridExtra")|]
  [r| require("scales") |]
  [r| require("RColorBrewer") |]
  [r|
    lt <- lbin_hs
    lf <- lsnr_hs
    tR0 <- fu_hs
    fR0 <- snrlist_hs
    tR <- rep(tR0,lf)
    fR <- sort(rep(fR0,lt))
    sgpR <- snrmap_hs
    df <- data.frame(T=tR,F=fR,C=log(sgpR))

 #   png('testpng.png',width=640,height=480,units="px",bg = "transparent")
    p <- ggplot(data=df,aes(x=T,y=F,fill=C))
     + geom_tile()
     + scale_fill_gradient(low="grey", high="red")
     + labs(x = "frequency[Hz]", y = "snr")
#     + geom_point(data=mkfd.fd,aes(x=xX, y=yY, color=cost)) +
#     scale_color_gradientn(colours = c('springgreen4', 'white')) +
#     theme( title = black.bold.text
#          , axis.title = black.bold.text
#          , axis.text = black.bold.text
#          , axis.ticks.length = unit(.1, "cm")
#          , axis.ticks = element_line(size = 1)
 #          , panel.background = element_rect(fill = "transparent",color = NA)
 #          , plot.background = element_rect(fill = "transparent",color = NA)
#          )
#         + xlim(0.03,0.35) + ylim(0,1000)
     ggsave( file = "snrHistHz.png"
           , plot = p
           , dpi = 100
           , width = 10
           , height = 8)

 #   dev.off()
    |]
  return ()
