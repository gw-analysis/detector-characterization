

import System.Environment (getArgs)
import HasKAL.WebUtils.FileWatcher (hrsync)

main = do
  (f, from, to) <- getArgs >>= \args -> case (length args) of
    3 -> return (head args, args!!1, args!!2)
    _ -> error " Usage: hrsync f from to"
  hrsync f from to

