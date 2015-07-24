


module HasKAL.Misc.Environment (
   haskalOpt
) where

import qualified System.Environment as SE
import qualified System.IO.Unsafe as SIOU


-- | Retern environment variable __${HASKALOPT}__
--
haskalOpt :: String
haskalOpt = SIOU.unsafePerformIO $ SE.getEnv "HASKALOPT"
