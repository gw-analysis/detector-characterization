{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UnicodeSyntax #-}


import Control.Monad.Trans
import Graphics.UI.Gtk hiding(Circle,Cross)
import qualified Graphics.Rendering.Cairo as C
--import qualified Graphics.Rendering.Pango as P
import Data.Colour.Names
import Numeric.LinearAlgebra
import Graphics.Rendering.Plot
import HasKAL.SpectrumUtils.DetectorSensitivity
import HasKAL.DetectorUtils.Detector

len = 1000
ts = linspace len (1::Double, 10000)
ds = ifonoisepsd LIGO ts

figure = do
--         setPlots 1 1
{-
         withPlot (1,1) $ do
                          setDataset [(Hist,hx,hy)]
                          addAxis XAxis (Side Lower) $ return ()
                          addAxis YAxis (Side Lower) $ return ()
-}{-                          setRange XAxis Lower (-4*pi) (1*pi)
                          setRange YAxis Lower (-4*pi) (1*pi) -}
{-                          setRange XAxis Lower 0 32
                          setRange YAxis Lower 0 20
-}
        --withLineDefaults $ setLineWidth 2
        withTextDefaults $ setFontFamily "OpenSymbol"
        withTitle $ setText "LIGO Design Sensitivity"
{-        withSubTitle $ do
                       setText "with 1 second of a 15Hz sine wave"
                       setFontSize 10
-}
        setPlots 1 1

        withPlot (1,1) $ do
--                         setDataset (Bar, lx, [hx,hy,he])
--                         barSetting BarStack
--                         setDataset (Line, mx, [rx])
--                         setDataset (Line, ts, [ds])
                         sampleData False
                         setDataset (ts,[line ds red])
--                         setDataset (ts,[impulse fs blue])
--                         setDataset (ts,[point (ds,es,"Sampled data") (Bullet,green)
--                         setDataset (ts,[bar (ds,ds+es,"Sampled data") green
--                                        ,line (fs,"15 Hz sinusoid") blue])
--                         setDataset [(Line,fx,fy)]
--                         setDataset ([bar (ds,es,"Sampled data") green
--                                     ,bar (gs,"Modified sample data") blue])
--                         setDataset (ts,[bar (ds,"Sampled data") green
--                                        ,line (fs,"15 Hz sinusoid") blue])
--                         setDataset [(Line,mx,my)]
--                         setDataset (Whisker,cx,[((cyl,cyu),(cel,ceu))])
--                         withAllSeriesFormats (\_ -> do
--                                                 setBarWidth 0.5
--                                                 setBarBorderWidth 0.1)
--                         setDataset (Hist,hx,[(hy,he)])
                         addAxis XAxis (Side Lower) $ do
                           --  setGridlines Major True
                           withAxisLabel $ setText "frequency (s)"
                           --setTicks Major (TickValues $ fromList [1,2,5,10])
                           setTicks Major (TickNumber 12)
                           setTicks Minor (TickNumber 100)
                           setTickLabelFormat $ Printf "%.2f"
                           --setTickLabels ["Jan","Feb","Mar","Apr","May"]
                           --withTickLabelFormat $ setFontSize 8
                         addAxis YAxis (Side Lower) $ do
--                           setGridlines Major True
                           withAxisLabel $ setText "powwer spectrum density (1/Hz)"
                           setTicks Minor (TickNumber 0)
                        -- addAxis XAxis (Value 0) $ return ()
                         setRangeFromData XAxis Lower Log
                         setRangeFromData YAxis Lower Log
--                         setRange XAxis Lower Linear 0 11
{-                         withAnnotations $ do
                           arrow True (pi/2,0.5) (0,0) (return ())
                           --oval True (0.5,1) (1,3) $ setBarColour blue
                           rect True (0.5,0.5) (2,0.75) $ (return ())
                           glyph (4,0.2) (return ())
                           text (3,0.0) (setText "from the α to the Ω")
                           cairo (\_ _ _ _ -> do
                                    C.moveTo 3 0.75
                                    C.lineTo 4 (-0.5)
                                    C.stroke
                                 )
-}
--                         setRange YAxis Lower Log (-1.25) 1.25
--                         setLegend True NorthEast Inside
--                         withLegendFormat $ setFontSize 6
{-
         withPlot (1,1) $ do
                          setDataset (ident 300 :: Matrix Double) --ms
                          addAxis XAxis (Side Lower) $ setTickLabelFormat "%.0f"
                          addAxis YAxis (Side Lower) $ setTickLabelFormat "%.0f"
                          setRangeFromData XAxis Lower
                          setRangeFromData YAxis Lower
-}

display :: ((Int,Int) -> C.Render ()) -> IO ()
display r = do
   initGUI       -- is start

   window <- windowNew
   set window [ windowTitle := "Simple Plot"
              , windowDefaultWidth := 640
              , windowDefaultHeight := 480
              , containerBorderWidth := 1
              ]

--   canvas <- pixbufNew ColorspaceRgb True 8 300 200
--   containerAdd window canvas
   frame <- frameNew
   containerAdd window frame
   canvas <- drawingAreaNew
   containerAdd frame canvas
   widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

   widgetShowAll window

   on canvas exposeEvent $ tryEvent $ do
     s <- liftIO $ widgetGetSize canvas
     drw <- liftIO $ widgetGetDrawWindow canvas
     --dat <- liftIO $ takeMVar d
     --liftIO $ renderWithDrawable drw (circle 50 10)
     liftIO $ renderWithDrawable drw (r s)

   onDestroy window mainQuit
   mainGUI


main = display $ render figure

test = writeFigure PDF "test.pdf" (640,480) figure
