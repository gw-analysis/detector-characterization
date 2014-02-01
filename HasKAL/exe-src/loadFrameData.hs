<<<<<<< HEAD:HasKAL/exe-src/loadFrameData.hs

import HasKAL.FrameUtils.FrameUtils

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- For complex number-}
--import Data.Complex

{- For plotting -}
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class



main :: IO (PickFn ())
main = do
  fdata <- readFrame "Channel_Name" "test-1066392016-300.gwf"
--  print $ show $ take 3 (eval fdata)
  let sampleRate = 1000::Double
  let dat = subVector 1 15000 (fromList $ map realToFrac (eval fdata))
      fft_val = fft $ dat
      power =  map abs $ toList $ fst . fromComplex $ fft_val * conj fft_val
      len_power = length power
      scale_psd = 1/((fromIntegral len_power) * sampleRate)
      len_power2 = floor $ fromIntegral(len_power)/2
      powerspectrum = take len_power2  $ map (*scale_psd) power

-- plotting --
      sampleRate2 = sampleRate/2
      fvec    = map (sampleRate2 *) $ toList (linspace len_power2 (0, 1::Double))
      plotLines = plot_lines_values .~ [(zip fvec (map sqrt powerspectrum))]
                  $ plot_lines_style . line_color .~ opaque red
                  $ plot_lines_title .~ "mag"
                  $ def
      layout    = layout_plots .~ [toPlot plotLines]
                  $ layout_title .~ "Magnetic field at the KAGRA site"
                  $ def

  renderableToPNGFile (toRenderable layout) 640 480  "test_magnetic.png"


=======

{-# LANGUAGE ScopedTypeVariables #-}

import HasKAL.FrameUtils.FrameUtils

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- For complex number-}
--import Data.Complex

{- For plotting -}
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart.Gtk
import Text.Printf



{- for renderableToPNGFile -}
-- main :: IO (PickFn ())
{- for renderableToWindow-}
--main :: IO()
main = do
  fdata <- readFrame "Channel_Name" "test-1066392016-300.gwf"
--  print $ show $ take 3 (eval fdata)
  let sampleRate = 1000::Double
  let dat = subVector 1 15000 (fromList $ map realToFrac (eval fdata))
      fft_val = fft $ dat
      power =  map abs $ toList $ fst . fromComplex $ fft_val * conj fft_val
      len_power = length power
      scale_psd = 1/((fromIntegral len_power) * sampleRate)
      len_power2 = floor $ fromIntegral(len_power)/2
      powerspectrum = take len_power2  $ map (*scale_psd) power

-- plotting --
      sampleRate2 = sampleRate/2
      fvec    = map (sampleRate2 *) $ toList (linspace len_power2 (0, 1::Double))
      plotLines = plot_lines_values .~ values
                  $ plot_lines_style . line_color .~ opaque red
                  $ plot_lines_title .~ "mag"
                  $ def
      layout    = layout_plots .~ [toPlot plotLines]
                  $ layout_title .~ "Magnetic field at the KAGRA site"
                  $ layout_title_style . font_size .~ 20
                  $ layout_x_axis . laxis_title .~ "frequency[Hz]"
                  $ layout_x_axis . laxis_style . axis_label_style . font_size .~ 20
                  $ layout_x_axis . laxis_title_style . font_size .~ 20
                  $ layout_y_axis . laxis_title .~ "spectrum[T/Hz]"
                  $ layout_y_axis . laxis_title_style . font_size .~ 20
                  $ layout_y_axis . laxis_style . axis_label_style . font_size .~ 20
                  $ layout_y_axis . laxis_generate .~ autoScaledLogAxis logPSDAxis
                  $ layout_legend .~ Just (legend_label_style . font_size .~ 20 $ def)
                  $ def
      values = [[(LogValue x, LogValue y)|(x,y)<- zip fvec (map sqrt powerspectrum)]]

--  renderableToWindow (toRenderable layout) 640 480
  renderableToPNGFile (toRenderable layout) 640 480  "test_magnetic.png"



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
    | k >= 1e-6  = (k*1e6)  `with` "µ"
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










>>>>>>> d4e5feb416a2dd90a8eaea8f75e11df8ba6e06fd:frameutil/exe-src/loadFrameData.hs
