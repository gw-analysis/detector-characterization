
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
--import HasKAL.FrameUtils.FrameUtils (readFrame)
import HasKAL.FrameUtils.Function (readFrameV)
import qualified Numeric.LinearAlgebra as NLA
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)

import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Storable as V (length)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
--import qualified Data.Complex as DC
--import Data.Complex (Complex( (:+) ))

--import HasKAL.DataBaseUtils.Function (kagraDataGet)
-- xs <- kagraDataGet 1124077565 64 K1:PEM-EX_MAG_X_FLOOR

main = do

 {-- configuration --} 
 let filename = "/data/kagra/xend/R0207/K-K1_R-1124077565-32.gwf"
     channel  = "K1:PEM-EX_MAG_X_FLOOR"
     fs = 2048 ::Double
 let tlist = [1/fs, 2/fs..32]

 {-- open frame file --}
 let channel  = "K1:PEM-EX_MAG_X_FLOOR"
     fs = 2048 ::Double
 let filename1 = "/data/kagra/xend/R0207/K-K1_R-1124077533-32.gwf"
     filename2 = "/data/kagra/xend/R0207/K-K1_R-1124077565-32.gwf"
     filename3 = "/data/kagra/xend/R0207/K-K1_R-1124077597-32.gwf"
     filename4 = "/data/kagra/xend/R0207/K-K1_R-1124077629-32.gwf"
     filename5 = "/data/kagra/xend/R0207/K-K1_R-1124077661-32.gwf"
     filename6 = "/data/kagra/xend/R0207/K-K1_R-1124077693-32.gwf"
     filename7 = "/data/kagra/xend/R0207/K-K1_R-1124077725-32.gwf"
     filename8 = "/data/kagra/xend/R0207/K-K1_R-1124077757-32.gwf"
     filename9 = "/data/kagra/xend/R0207/K-K1_R-1124077789-32.gwf"
     filename10 = "/data/kagra/xend/R0207/K-K1_R-1124077821-32.gwf"
 maybexs1 <- readFrameV channel filename1
 let xs1 = fromMaybe (error " no data in the file.") maybexs1
 maybexs2 <- readFrameV channel filename2
 let xs2 = fromMaybe (error " no data in the file.") maybexs2
 maybexs3 <- readFrameV channel filename3
 let xs3 = fromMaybe (error " no data in the file.") maybexs3
 maybexs4 <- readFrameV channel filename4
 let xs4 = fromMaybe (error " no data in the file.") maybexs4
 maybexs5 <- readFrameV channel filename5
 let xs5 = fromMaybe (error " no data in the file.") maybexs5
 maybexs6 <- readFrameV channel filename6
 let xs6 = fromMaybe (error " no data in the file.") maybexs6
 maybexs7 <- readFrameV channel filename7
 let xs7 = fromMaybe (error " no data in the file.") maybexs7
 maybexs8 <- readFrameV channel filename8
 let xs8 = fromMaybe (error " no data in the file.") maybexs8
 maybexs9 <- readFrameV channel filename9
 let xs9 = fromMaybe (error " no data in the file.") maybexs9
 maybexs10 <- readFrameV channel filename10
 let xs10 = fromMaybe (error " no data in the file.") maybexs10
 let xs = xs1 DVG.++ xs2 DVG.++ xs3 DVG.++ xs4 DVG.++ xs5 DVG.++ xs6 DVG.++ xs7 DVG.++ xs8 DVG.++ xs9 DVG.++ xs10

 {-- fft xs data --}
 let hoff1 = gwpsdV xs1 (V.length xs1) fs
 let hoff2 = gwpsdV xs2 (V.length xs2) fs
 let hoff3 = gwpsdV xs3 (V.length xs3) fs
 let hoff4 = gwpsdV xs4 (V.length xs4) fs
 let hoff5 = gwpsdV xs5 (V.length xs5) fs
 let hoff6 = gwpsdV xs6 (V.length xs6) fs
 let hoff7 = gwpsdV xs7 (V.length xs7) fs
 let hoff8 = gwpsdV xs8 (V.length xs8) fs
 let hoff9 = gwpsdV xs9 (V.length xs9) fs
 let hoff10 = gwpsdV xs10 (V.length xs10) fs

 {-- sum each frequecy band --}
 {-- frequency band => index of vector --}
 {-- [0.2:1] [1:4] [4:10] Hz --}

 {-- plot RMS value --}
 {-- plotX :: LogOption -> PlotTypeOption -> Int -> ColorOpt -> (String, String) -> Double -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double)] -> IO ()
     plotX log mark lineWidth color xyLable labelSize title range dat  --}

-- plotX Linear Dot 1 RED ("time", channel) 0.08 "RMSMon" ((0,0), (0,0)) $ zip tlist xs

 let hoff1 = gwpsdV xs1 (V.length xs1) fs
 let hoff2 = gwpsdV xs2 (V.length xs2) fs
-- let hoff  = DVG.zip hoff1 hoff2
 oPlotXV LogXY Dot 1 [BLUE, RED] ("frequency [Hz]", "[/rHz]") 0.05 "title" ((0,0),(0,0)) [hoff1, hoff2]

 --plotXV LogXY LinePoint 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 "title" ((0,0),(0,0)) hoff
-- plotXV Linear Dot 1 RED ("time", channel) 0.08 "RMSMon" ((0,0), (0,0)) $ DVG.zip (DVG.fromList tlist) xs


 return 0





threedata2string ::[Double] -> [Double] ->[Double]-> String
threedata2string a b c = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  (map show c)

sixdata2string::[Double] -> [Double] ->[Double]->[Double]->[Double]->[Double]-> String
sixdata2string a b c d e f = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show c) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show d) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show e) $ zipWith (++) (repeat " ")  ( map show f)

