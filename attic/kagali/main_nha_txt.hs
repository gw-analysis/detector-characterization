
import qualified Data.Vector.Storable as VS
import Data.List
import Numeric
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils as KGL 


main :: IO()
main = do
--     cnsig <- getArgs
     string <- readFile "../../HasKAL/src/HasKAL/LineUtils/LineRemoval/LIGOtest.dat"
     let stringList = lines string
         frameList = makeDouble stringList
         frameV = VS.fromList frameList
--         nsig = read (cnsig!!0) :: Int
         nsig   = 5      :: Int
         order  = 6      :: Int
         fs     = 2048   :: Double
         fmin   = 300    :: Double
         fmax   = 400    :: Double
         nframe = 1024   :: Int
         nshift = 32     :: Int
         nstart = 0      :: Int
         nend   = 30000  :: Int
         output = KGL.butterBandPass frameV fs fmin fmax order
     case output of 
       Left message -> print message 
       Right frameV_bp -> do
         let outV = KGL.nha frameV_bp fs nsig nframe nshift nstart nend
             outText = concat $ map (toText . shift) outV
         writeFile "LIGOtest.ana" $ outText

makeDouble :: [String] -> [Double]
makeDouble = map read


toText :: [[Double]] -> String
toText xss = unlines . map (unwords . map (\x -> Numeric.showEFloat (Just 10) x "") ) . transpose $ xss 

{-
shift :: (Double, Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (tstart, tend, x, y, z) = [tstart', tend', VS.toList x, VS.toList y, VS.toList z]
  where tstart' = take num $ repeat tstart
        tend' = take num $ repeat tend
        num = VS.length x
-}

shift :: (Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (time, x, y, z) = [time', VS.toList x, VS.toList y, VS.toList z]
  where time' = take num $ repeat time
        num = VS.length x
