{- |
Module      : HasKAL.Misc.Environment
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

HasKAL environment
-}

module HasKAL.Misc.Environment (
   haskalOpt
) where

import qualified System.Environment as SE
import qualified System.IO.Unsafe as SIOU


-- | Retern environment variable __${HASKALOPT}__
--
haskalOpt :: String
haskalOpt = SIOU.unsafePerformIO $ SE.getEnv "HASKALOPT"
