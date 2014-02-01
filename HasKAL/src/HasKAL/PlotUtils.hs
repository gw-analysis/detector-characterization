

{-

example:

:m System.Random
:l HasKAL_PlotUtils.hs
let vals3d = take 20 $ randomRs (-2,27) $ mkStdGen 1 :: [Double]
let valz3d = take 20 $ randomRs (-2,27) $ mkStdGen 1 :: [Double]
let dat3d = zip3 [1..20::Double] vals3d valz3d
scatter_plot_3d_png "kleineWelle triggers" "triggered event" 10 "example3d.png" dat3d
scatter_plot_3d "kleineWelle triggers" "triggered event" 10 (640,480) dat3d


:m System.Random
:l HasKAL_PlotUtils.hs
let vals2d = take 20 $ randomRs (-2,27) $ mkStdGen 1 :: [Double]
let dat2d = zip [1..20::Double] vals2d
scatter_plot_2d_png "kleineWelle triggers" "triggered event" 10 "example2d.png" dat2d
scatter_plot_2d "kleineWelle triggers" "triggered event" 10 (640,480) dat2d

-}


{-# LANGUAGE ScopedTypeVariables #-}

module HasKAL.PlotUtils
    (scatter_plot_2d
    ,scatter_plot_2d_png
    ,scatter_plot_3d
    ,scatter_plot_3d_png
    ) where


import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Text.Printf


scatter_plot_2d :: String -> String -> Double -> (Int,Int) -> [(Double,Double)] -> IO ()
scatter_plot_2d str_title str_legend lwidth windowSize dat =
    renderableToWindow (scatter_plot_2d_internal str_title str_legend lwidth dat) (fst windowSize) (snd windowSize)

scatter_plot_2d_png :: String -> String -> Double -> FilePath -> [(Double,Double)] -> IO (PickFn ())
scatter_plot_2d_png str_title str_legend lwidth plotfname dat =
    renderableToFile def (scatter_plot_2d_internal str_title str_legend lwidth dat) plotfname

scatter_plot_3d :: String -> String -> Double -> (Int,Int) -> [(Double,Double,Double)] -> IO ()
scatter_plot_3d str_title str_legend lwidth windowSize dat =
    renderableToWindow (scatter_plot_3d_internal str_title str_legend lwidth dat) (fst windowSize) (snd windowSize)

scatter_plot_3d_png :: String -> String -> Double -> FilePath -> [(Double,Double,Double)] -> IO (PickFn ())
scatter_plot_3d_png str_title str_legend lwidth plotfname dat =
    renderableToFile def (scatter_plot_3d_internal str_title str_legend lwidth dat) plotfname



{- internal function -}
scatter_plot_2d_internal :: String -> String -> Double -> [(Double,Double)] -> Renderable ()
scatter_plot_2d_internal str_title str_legend lwidth dat = toRenderable layout
  where
    layout = layout_plots .~ [toPlot spots]
           $ layout_title .~ str_title
           $ layout_title_style . font_size .~ 20
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_left_axis_visibility . axis_show_ticks .~ False
           $ setLayoutForeground (opaque black)
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 20
           $ layout_x_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_title .~ "y"
           $ layout_y_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 20
    --           $ layout_y_axis . laxis_generate .~ autoScaledLogAxis logPSDAxis
           $ layout_legend .~ Just (legend_label_style . font_size .~ 20 $ def)
           $ def

    spots = area_spots_title .~ str_legend
           $ area_spots_max_radius .~ lwidth
           $ area_spots_fillcolour .~ blue
           $ area_spots_values .~ values
           $ def

    values = [ (d, v, z) | ((d,v),z) <- zip dat zs ]
    zs :: [Int]
    zs     = repeat $ round lwidth


scatter_plot_3d_internal :: String -> String -> Double -> [(Double,Double,Double)] -> Renderable ()
scatter_plot_3d_internal str_title str_legend lwidth dat = toRenderable layout
  where
    layout = layout_plots .~ [toPlot spots]
           $ layout_title .~ str_title
           $ layout_title_style . font_size .~ 20
           $ layout_background .~ solidFillStyle (opaque white)
           $ layout_left_axis_visibility . axis_show_ticks .~ False
           $ setLayoutForeground (opaque black)
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 20
           $ layout_x_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_title .~ "y"
           $ layout_y_axis . laxis_title_style . font_size .~ 20
           $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 20
--           $ layout_y_axis . laxis_generate .~ autoScaledLogAxis logPSDAxis
           $ layout_legend .~ Just (legend_label_style . font_size .~ 20 $ def)
           $ def

    spots = area_spots_4d_title .~ str_legend
          $ area_spots_4d_max_radius .~ lwidth
          $ area_spots_4d_values .~ values
          $ def

    values = [ (d, v, z, t) | ((d,v,t),z) <- zip dat zs ]
    zs :: [Int]
    zs     = repeat $ round lwidth


{-

Helper function

-}

logPSDAxis = loga_labelf .~ (mags . fromLogValue) $ def
mags :: Double -> String
mags  k
    | k < 0      = '-' : mags (-k)
    | k >= 1e9   = (k/1e9)  `with` "G"
    | k >= 1e6   = (k/1e6)  `with` "M"
    | k >= 1e4   = (k/1e3)  `with` "K"
    | k >= 1     = k        `with` " "
    | k >= 1e-3  = (k*1e3)  `with` "m"
    | k >= 1e-6  = (k*1e6)  `with` "袖"
    | k >= 1e-9  = (k*1e9)  `with` "n"
    | k >= 1e-12 = (k*1e12) `with` "p"
    | otherwise  = printf "%g s" k
    where with (t :: Double) (u :: String)
              | t >= 1e9  = printf "%.4g %s" t u
              | t >= 1e6  = printf "%.0f %s" t u
              | t >= 1e5  = printf "%.0f %s" t u
              | t >= 1e4  = printf "%.0f %s" t u
              | t >= 1e3  = printf "%.0f %s" t u
              | t >= 1e2  = printf "%.0f %s" t u
              | t >= 1e1  = printf "%.0f %s" t u
              | otherwise = printf "%.0f %s" t u

fromLogValue (LogValue v) = v

