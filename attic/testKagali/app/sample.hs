
import qualified Data.Vector.Storable as VS
import Data.List
import Numeric
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils as KGL
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils as KGL2

main :: IO()
main = do
--     cnsig <- getArgs
     string <- readFile "LIGOtest.dat"
     let stringList = lines string
         frameList = makeDouble stringList
         frameV = VS.fromList frameList
         fs     = 2048   :: Double
         nframe = 256    :: Int
         nshift = 128    :: Int
         nstart = 0      :: Int
         nend   = 3000   :: Int
         t0     = 0      :: Double
         alpha  = 5      :: Double
         ipath  = 5      :: Int
         output = KGL2.dKGLChirpletMain frameV fs alpha ipath
         timeV = [0,1/fs..(fromIntegral nframe-1)/fs]
     let (freqV,c) = output
     print $ take 5 $ VS.toList freqV
     -- mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) $ zip (V.toList vf) (V.toList vasd)

makeDouble :: [String] -> [Double]
makeDouble = map read

toText :: [[Double]] -> String
toText xss = unlines . map (unwords . map (\x -> Numeric.showEFloat (Just 10) x "") ) . transpose $ xss

shift :: (Double, VS.Vector Double, VS.Vector Double, VS.Vector Double) -> [[Double]]
shift (time, x, y, z) = [time', VS.toList x, VS.toList y, VS.toList z]
  where time' = take num $ repeat time
        num = VS.length x
