
import System.Environment (getArgs)
import HasKAL.FrameUtils.FileManipulation (genFileList)

main=do
  args <- getArgs
  (cacheFile, absDir) <- case (length args) of
                          2 -> return (args!!0, args!!1)
                          _ -> error "Usage: generateCacheFile outputFile absDir"

  genFileList cacheFile absDir

