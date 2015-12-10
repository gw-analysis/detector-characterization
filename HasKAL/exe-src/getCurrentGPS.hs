


import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import System.IO (hFlush,stdout)

main = do
 getCurrentGps >>= \gps -> putStrLn gps >> hFlush stdout

