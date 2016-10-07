import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import System.Environment(getArgs)

import HasKAL.IOUtils.Function (stdin2vec)
import qualified Data.Vector.Storable as V
import System.IO.Unsafe (unsafePerformIO)

plotHistCore title xmin xmax nbin fun dat = toRenderable layout
 where
  layout = 
        layout_title .~ " " ++ btitle
      $ layout_title_style . font_size .~ 20
      $ layout_x_axis . laxis_generate .~ autoIndexAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridHide
      $ layout_left_axis_visibility . axis_show_ticks .~ False
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex Double

  bars2 = plot_bars_titles .~ [title]
      $ plot_bars_values .~ addIndexes vals
      $ plot_bars_style .~ BarsClustered
      $ plot_bars_spacing .~ BarsFixGap 1 5
      $ plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq)
      $ def

  alabels = map show x 
  vals = [[a]|a<-y]
  dh = (xmax-xmin)/fromIntegral nbin
  nbins = [xmin,xmin+dh..xmax]
  (x,y) = fun xmin xmax nbins dat
  
  borders = True
  btitle = if borders then "" else " (no borders)"
  bstyle = if borders then Just (solidLine 1.0 $ opaque black) else Nothing
  mkstyle c = (solidFillStyle c, bstyle)


histogram1d :: Double -> Double -> [Double] -> [Double] -> ([Double], [Double])
histogram1d xmin xmax bins input =
  let intervals = zipWith (\x y ->(x, y)) (init bins) (tail bins)
      within u x = x >= fst u && x < snd u
   in (map fst intervals, map ((fromIntegral . length) . (\u -> filter (within u) input)) intervals)


main =  do 
  (title, xmin',xmax',nbin') <- getArgs >>= \args -> case (length args) of
    4 -> return (head args,args!!1,args!!2,args!!3)
    _ -> error "Usage: plotHistgram title min max #bin STDIN"
  let dat = V.toList $ unsafePerformIO stdin2vec
      xmin = read xmin' :: Double
      xmax = read xmax' :: Double
      nbin = read nbin' :: Int
      plotfname = "histogram"++"-"++xmin'++"_"++xmax'++"_"++nbin'++".png"
  renderableToFile def plotfname (plotHistCore title xmin xmax nbin histogram1d dat)


