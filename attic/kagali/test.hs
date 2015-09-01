


import qualified Data.Vector.Storable as VS
import KAGALIUtils

main :: IO()
main = do
  let inputV = VS.fromList [1, 2, 3, (4::Double)]
      outputV = dKGLInferenceSamplefn inputV
  print $ take 2 $ VS.toList outputV

