
import qualified Data.Vector.Storable as VS
import Data.List
import Numeric
import KAGALIUtils
--import System.Environment


main :: IO()
main = do
--     cnsig <- getArgs
     string <- readFile "../../HasKAL/src/HasKAL/LineUtils/LineRemoval/LIGOtest.dat"
     let stringList = lines string
         frameList = makeDouble stringList
         frameV = VS.fromList frameList
--         nsig = read (cnsig!!0) :: Int
         nsig = 5
         fs = 2048.0
         nframe = 256
         nshift = 64
         nstart = 0
         nend = 30000
         outV = nha frameV fs nsig nframe nshift nstart nend
         outText = concat $ map (toText . shift) outV
     writeFile "output.dat" $ outText


makeDouble :: [String] -> [Double]
makeDouble = map read


nha :: VS.Vector Double -> Double -> Int -> Int -> Int -> Int -> Int -> [(Double, Double, VS.Vector Double, VS.Vector Double, VS.Vector Double)]
nha datV fs nsig nframe nshift nstart nend = retVal
  where retVal = zipWith3 (\v w (x, y, z) -> (v, w, x, y, z)) tstart tend result
        tstart = map ( (/fs) . fromIntegral ) nIdx
        tend = map ( (/fs) . fromIntegral . (+nframe) ) nIdx
        nIdx = [nstart, nstart + nshift .. nstop]
        nstop = min (VS.length datV - nframe) nend
        result =
          map ( (\frameV -> dKGLIterativeLeastSquare2DNewton frameV fs nsig) . (\kstart -> VS.slice kstart nframe datV) ) nIdx


toText :: [[Double]] -> String
toText xss = unlines . map (unwords . map (\x -> Numeric.showEFloat (Just 10) x "") ) . transpose $ xss 


shift :: (Double, Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (tstart, tend, x, y, z) = [tstart', tend', VS.toList x, VS.toList y, VS.toList z]
  where tstart' = take num $ repeat tstart
        tend' = take num $ repeat tend
        num = VS.length x

