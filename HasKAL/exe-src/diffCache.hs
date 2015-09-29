
import Data.List ((\\), delete)
import Control.Monad (liftM)
import System.Environment (getArgs)

import qualified Data.ByteString.Char8 as BS

main = do
  args <- getArgs
  (file1, file2) <- case (length args) of
                     2 -> return (args!!0, args!!1)
                     _ -> error "Usage: diffCache newfile oldfile"

  [cache1, cache2] <- mapM (liftM lines.readFile) [file1, file2]

  mapM putStrLn $ cache1 \\ cache2

setDifference xs ys = map BS.unpack $ foldl (flip delete) xs' ys'
  where xs' = map BS.pack xs
        ys' = map BS.pack ys

