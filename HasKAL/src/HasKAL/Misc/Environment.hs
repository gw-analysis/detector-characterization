{-******************************************
  *     File Name: Environment.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/13 16:01:05
  *******************************************-}

module HasKAL.Misc.Environment (
   haskalOpt
) where

import qualified System.Environment as SE
import qualified System.IO.Unsafe as SIOU

haskalOpt :: String
haskalOpt = SIOU.unsafePerformIO $ SE.getEnv "HASKALOPT"
