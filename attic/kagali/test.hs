
import qualified Data.Vector.Storable as VS
import KAGALIUtils

main :: IO()
main = do
  let inputV = VS.fromList [1, 2, 3, (4::Double)]
      outputV = dKGLInferenceSamplefn inputV
  putStrLn "input" :: IO ()
  print $ VS.toList inputV
  putStrLn "output" :: IO ()
  print $ take 4 $ VS.toList outputV
