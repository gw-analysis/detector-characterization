

import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as NL
import System.Random (randomIO)
import System.IO.Unsafe (unsafePerformIO)
import FFTW


main = do
  seed <- randomIO
  let v = NL.randomVector seed NL.Gaussian 4 :: VS.Vector Double
  let mIn = randnM 3 3
  disp mIn
  print $ VS.toList v

  print "DCT"
  let dcted = dct1d v
  print $ VS.toList dcted
  print "IDCT"
  let idcted = idct1d dcted
  print $ VS.toList idcted

  print "DCT2d"
  let dct2ded = dct2d mIn
  disp dct2ded
  print "IDCT2d"
  let idct2ded = idct2d dct2ded
  disp idct2ded

  print "DFTRH1d"
  let dfted = dftRH1d v
  print $ VS.toList dfted
  print "DFTHR1d"
  let idfted = dftHR1d dfted
  print $ VS.toList idfted

  print "DFTRH2d"
  let dft2ded = dftRH2d mIn
  disp dft2ded
  let idft2ded = dftHR2d dft2ded
  print "DFTHR2d"
  disp idft2ded


randnM r c = unsafePerformIO $ do
  seed <- randomIO
  return (NL.reshape c $ NL.randomVector seed NL.Gaussian (r*c))

disp = putStr . NL.dispf 2


