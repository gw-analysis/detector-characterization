
import Data.List (delete)
import Data.Packed.Vector (fromList, dim)
import System.Environment (getArgs)

import HasKAL.FrameUtils.FrameUtils (getGPSTime, getSamplingFrequency)
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.PlotUtils.HROOT.PlotGraph


main = do
  {-- parameters --}
  args <- getArgs
  let (chname, filename, flag) = case (length args, elem "-X" args) of
        (3, True) -> ((delete "-X" args)!!0, (delete "-X" args)!!1, True)
        (2, False) -> (args!!0, args!!1, False)
        (_, _) -> error "Usage: plottimeseries [-X] chname filename"

  {-- read data --}
  mb_dat <- readFrameV chname filename
  dat <- case mb_dat of
          Nothing -> error "Can't find file or channel."
          Just dat -> return dat

  mb_fs <- getSamplingFrequency filename chname
  fs <- case mb_fs of
         Nothing -> error "Can't read sampling rate."
         Just fs -> return fs

  mb_gps <- getGPSTime filename
  gps <- case mb_gps of
          Nothing -> error "Can't read GPS time."
          Just (gpsS,_,_) -> return $ show gpsS

  (title, ofile) <- case flag of
                     True  -> return (gps++": "++chname, "X11")
                     False -> return (gps++": "++chname, chname++"_"++gps++"-TS.png")

  {-- main --}
  let tvec = fromList [0, 1/fs..(fromIntegral $ dim dat) /fs]
  plotV Linear Line 1 RED ("time [s]", "amplitude") 0.05 title ofile ((0,0),(0,0)) (tvec, dat)

