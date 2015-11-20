

import System.Environment (getArgs)

import HasKAL.TimeUtils.GPSfunction (gps2localTime)

main = do
  {-- parameters --}
  args <- getArgs
  (gps, locate) <- case (length args) of
                    1 -> return $ (args!!0, "UTC")
                    2 -> return $ (args!!0, args!!1)
                    _ -> error message

  {-- main --}
  let localT = gps2localTime (read gps) locate
  putStrLn $ fillspc 6 "GPS"  ++ ": " ++ gps
  putStrLn $ fillspc 6 locate ++ ": " ++ localT

fillspc :: Int -> String -> String
fillspc n str = replicate m ' ' ++ str
  where m = n - length str


message :: String
message = concat [
  "Usage: gps2localtime GPS [ST]\n",
  "     ST:: JST, UTC ..."
  ]
