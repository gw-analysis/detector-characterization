
import qualified Data.Vector.Storable as VS
import Data.List
import KAGALIUtils

main :: IO()
main = do
     string <- readFile "test.dat"
     let stringList = lines string
         frameList = makeDouble stringList
         frame = VS.fromList frameList
         fs = 2048.0
         dt = 1.0/fs
         nframe = length frameList
         time = take (nframe) [0,dt..]
         nsig = 2
         (out1,out2,out3) = dKGLIterativeLeastSquare2DNewton frame fs nsig
         out1List = VS.toList out1
         out2List = VS.toList out2
         out3List = VS.toList out3
         outText = toText [out1List, out2List, out3List]
         
     print $ dt
--     print $ time
     writeFile "output.dat" $ outText
     
     
makeDouble :: [String] -> [Double]
makeDouble = map read

toText :: [[Double]] -> String
toText xss = unlines . map (unwords . map show) . transpose $ xss 
