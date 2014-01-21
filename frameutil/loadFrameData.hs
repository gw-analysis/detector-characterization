
import HasKAL_FrameUtils

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


