{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import qualified Numeric.LinearAlgebra as NL
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
      let inputPart = unsafePerformIO $ stdin2vec
          inputlst = V.toList inputPart
      hPutStrLn stdout (show (mean inputlst))


mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m,  !n) x -> (m+(x-m)/(n+1), n+1)) (0, 0) x
