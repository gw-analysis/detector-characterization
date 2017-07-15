-- #!/usr/bin/env stack
-- -- stack --resolver=lts-5.2 runghc

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import System.Process
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main :: IO ()
main = do
  args <- getArgs
  case length args  of
    2 -> do let opt = head args
                fname = args !! 1
            s <- readProcess "od" ["-t", opt, fname] []
            mapM_ (hPutStrLn stdout)  $ concatMap (drop 1) $ filter (\x ->length x == 3) $ map words (lines s)
    _ -> error "Usage: gif_bin2stdout fD(fF) filepath"
