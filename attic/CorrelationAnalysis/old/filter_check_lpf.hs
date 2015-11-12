import Numeric.LinearAlgebra  --subVector
import Numeric --showFFloat
import Control.Monad -- forM
import System.Environment -- getArgs
import System.Random
import System.Process -- system
import Data.List.Split -- splitOn

import HasKAL.FrameUtils.FrameUtils -- read Frame file

import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as PMOP
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType

main = do


 let fileName = "/home/yuzurihara/frame/clio/X-R-1034657664-16.gwf"
 let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
 let dfs = 16384 ::Double
 readData1 <- readFrame channelName (fileName)
 let data1  = map realToFrac (eval readData1)
     tt = map (/dfs) $ take (length data1) [1,2..]

-- resampling from 16384Hz to 2048Hz
-- low-pass filter of cutoff frequency = 1024Hz
-- let dataLPF = butter 4 dfs (2048::Double) Low
 let n=4::Int
 let (numCoeffLow,denomCoeffLow) = butter n dfs 1024.0 Low :: ([Double], [Double])
     dataFiltered = iirFilter data1 numCoeffLow denomCoeffLow

 PM.oPlot PMOP.Linear PMOP.Line ("time [sec]", "v(t)") (repeat "check for filted data") "op_lpf_result.png"$ [zip tt data1, zip tt dataFiltered]
 PM.plot PMOP.Linear PMOP.Line ("time [sec]", "v(t)") ("check for filted data") "no_filter.png" $ zip tt data1
 print "hoge"
