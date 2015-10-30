

import System.Environment (getArgs)
import Text.Regex (mkRegex, splitRegex)

import HasKAL.TimeUtils.GPSfunction (time2gps)

main = do
  {-- parameters --}
  args <- getArgs
  let strings = case length args' of
                 7 -> args'
                 _ -> error message
                where args' = concat $ map (splitRegex (mkRegex ":")) $ concat $ map (splitRegex (mkRegex "-")) args

  {-- main --}
  let (localT, locate) = format strings
      gps = time2gps localT
  putStrLn $ fillspc 6 locate ++ ": " ++ localT
  putStrLn $ fillspc 6 "GPS"  ++ ": " ++ gps

format :: [String] -> (String, String)
format strs = (yyyy ++ "-" ++ mm ++ "-" ++ dd ++ " " ++ hH ++ ":" ++ mM ++ ":" ++ sS ++ " " ++ locate, locate)
  where yyyy = show0 4 (strs!!0)
        mm = show0 2 (strs!!1)
        dd = show0 2 (strs!!2)
        hH = show0 2 (strs!!3)
        mM = show0 2 (strs!!4)
        sS = show0 2 (strs!!5)
        locate = case (strs!!6) of
                  "JST" -> "JST"
                  "UTC" -> "UTC"
                  _     -> "UTC"

show0 :: Int -> String -> String
show0 n x 
  | len < n   = replicate (n-len) '0' ++ x
  | otherwise = x
  where len = length x

fillspc :: Int -> String -> String
fillspc n str = replicate m ' ' ++ str
  where m = n - length str

message :: String
message = concat [
  "Usage: localtime2gps DATE ST\n",
  "   DATE:: yyyy-mm-dd HH:MM:SS\n",
  "          yyyy mm dd HH MM SS\n",
  "     ST:: JST, UTC ..."
  ]
