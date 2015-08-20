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

 {-- open frame file --}
 let channel  = "K1:PEM-EX_MAG_X_FLOOR"
     fs = 2048 ::Double
 let filelist = ["/data/kagra/xend/R0207/K-K1_R-1124077533-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077565-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077597-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077629-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077661-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077693-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077725-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077757-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077789-32.gwf","/data/kagra/xend/R0207/K-K1_R-1124077821-32.gwf"]

 maybexs <- mapM (readFrameV channel) filelist
 let xs = map (fromMaybe (error " no data in the file.")) maybexs
 --let ys = map (DVG.++) xs
-- print ys
 print $ length filelist

 let nchunk = 32 * 2048 ::Int

 let tlist = [1/fs, 2/fs..32]

 {-- fft xs data --}
 let hoff1 = gwpsdV (xs!!0 ) nchunk fs
 print $ DVG.take 100 $ fst hoff1
 print $ DVG.take 100 $ snd hoff1

 --let a = DVG.zip (NLA.fromList [0.0,1.0..]) (fst hoff1)

 -- let hoff1 = gwpsdV (DVG.slice 0 nchunk ys) nchunk fs
 -- let hoff2 = gwpsdV xs2 (V.length xs2) fs
 -- let hoff3 = gwpsdV xs3 (V.length xs3) fs
 -- let hoff4 = gwpsdV xs4 (V.length xs4) fs
 -- let hoff5 = gwpsdV xs5 (V.length xs5) fs
 -- let hoff6 = gwpsdV xs6 (V.length xs6) fs
 -- let hoff7 = gwpsdV xs7 (V.length xs7) fs
 -- let hoff8 = gwpsdV xs8 (V.length xs8) fs
 -- let hoff9 = gwpsdV xs9 (V.length xs9) fs
 -- let hoff10 = gwpsdV xs10 (V.length xs10) fs

--  {-- sum each frequecy band --}
--  {-- frequency band => index of vector --}
--  {-- [0.2:1] [1:4] [4:10] Hz --}

--  {-- plot RMS value --}
-- -- plotX Linear Dot 1 RED ("time", channel) 0.08 "RMSMon" ((0,0), (0,0)) $ zip tlist xs

--  let hoff1 = gwpsdV xs1 (V.length xs1) fs
--  let hoff2 = gwpsdV xs2 (V.length xs2) fs
-- -- let hoff  = DVG.zip hoff1 hoff2
--  oPlotXV LogXY Dot 1 [BLUE, RED] ("frequency [Hz]", "[/rHz]") 0.05 "title" ((0,0),(0,0)) [hoff1, hoff2]



-- plotXV LogXY LinePoint 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 "title" ((0,0),(0,0)) hoff1


-- plotXV Linear Dot 1 RED ("time", channel) 0.08 "RMSMon" ((0,0), (0,0)) $ DVG.zip (DVG.fromList tlist) xs


 return 0



threedata2string ::[Double] -> [Double] ->[Double]-> String
threedata2string a b c = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  (map show c)

sixdata2string::[Double] -> [Double] ->[Double]->[Double]->[Double]->[Double]-> String
sixdata2string a b c d e f = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show c) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show d) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show e) $ zipWith (++) (repeat " ")  ( map show f)

