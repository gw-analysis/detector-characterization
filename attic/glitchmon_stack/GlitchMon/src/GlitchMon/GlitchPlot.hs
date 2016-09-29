


module GlitchMon.GlitchPlot
( scatterplot
, scatterplot'
, scatterplot'png
, scatterplot'png'
) where



import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Data.Colour.Palette.BrewerSet


scatterplot'png :: String -> String -> FilePath -> [(Double,Double,Double)] -> IO (PickFn ())
scatterplot'png str_title str_legend plotfname dat =
    renderableToFile def plotfname (scatterplot_internal str_title str_legend dat)


scatterplot :: String -> String -> (Int,Int) -> [(Double,Double,Double)] -> IO ()
scatterplot str_title str_legend windowSize dat =
    renderableToWindow (scatterplot_internal str_title str_legend dat) (fst windowSize) (snd windowSize)


scatterplot'png' :: String -> String -> FilePath -> [(Double,Double,Double,Int)] -> IO (PickFn ())
scatterplot'png' str_title str_legend plotfname dat =
    renderableToFile def plotfname (scatterplot_internal' str_title str_legend dat)


scatterplot' :: String -> String -> (Int,Int) -> [(Double,Double,Double,Int)] -> IO ()
scatterplot' str_title str_legend windowSize dat =
    renderableToWindow (scatterplot_internal' str_title str_legend dat) (fst windowSize) (snd windowSize)


scatterplot_internal :: String -> String -> [(Double,Double,Double)] -> Renderable ()
scatterplot_internal str_title str_legend dat = toRenderable layout
  where
    layout = layout_plots .~ [toPlot spots]
           $ layout_title .~ str_title
           $ layout_title_style . font_size .~ 20
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_left_axis_visibility . axis_show_ticks .~ False
           $ layout_foreground .~ (opaque black)
           $ layout_x_axis . laxis_title .~ "time"
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 20
           $ layout_x_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_title .~ "frequency [Hz]"
           $ layout_y_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 20
--           $ layout_y_axis . laxis_generate .~ autoScaledLogAxis logPSDAxis
           $ layout_legend .~ Just (legend_label_style . font_size .~ 20 $ def)
           $ def

    spots = area_spots_4d_title .~ str_legend
          $ area_spots_4d_max_radius .~ 20
          $ area_spots_4d_values .~ values
          $ area_spots_4d_palette .~ brewerSet RdYlBu 11
          $ def

    values = [ (d, v, setColor (t*z), 5*sqrt t) | ((d,v,t),z) <- zip dat zs ]
    zs :: [Double]
    zs     = repeat $ 1


scatterplot_internal' :: String 
                      -> String 
                      -> [(Double,Double,Double,Int)] 
                      -> Renderable ()
scatterplot_internal' str_title str_legend dat = toRenderable layout
  where
    layout = layout_plots .~ [toPlot spots]
           $ layout_title .~ str_title
           $ layout_title_style . font_size .~ 20
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_left_axis_visibility . axis_show_ticks .~ False
           $ layout_foreground .~ (opaque black)
           $ layout_x_axis . laxis_title .~ "time"
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 20
           $ layout_x_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_title .~ "frequency [Hz]"
           $ layout_y_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 20
--           $ layout_y_axis . laxis_generate .~ autoScaledLogAxis logPSDAxis
           $ layout_legend .~ Just (legend_label_style . font_size .~ 20 $ def)
           $ def

    spots = area_spots_4d_title .~ str_legend
          $ area_spots_4d_max_radius .~ 30
          $ area_spots_4d_values .~ values
          $ area_spots_4d_palette .~ brewerSet YlOrRd 9
          $ def

    values = [ (d, v, sqrt (fromIntegral n :: Double), setColor' (t*z)) | ((d,v,t,n),z) <- zip dat zs ]
    zs :: [Double]
    zs = repeat $ 1



setColor :: Double -> Int
setColor x
  | x < 3 = 0
  | x < 5 = 1
  | x < 7 = 2
  | x < 9 = 3
  | x < 11 = 4
  | x < 13 = 5
  | x < 15 = 6
  | x < 17 = 7
  | x < 19 = 8
  | x < 30 = 9
  | x < 60 = 10
  | otherwise = 11


setColor' :: Double -> Int
setColor' x
  | x < 3 = 0
  | x < 5 = 1
  | x < 8 = 2
  | x < 10 = 3
  | x < 15 = 4
  | x < 20 = 5
  | x < 25 = 6
  | x < 30 = 7
  | x < 50 = 8
  | otherwise = 9

