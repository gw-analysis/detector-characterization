{-
get tai-utc.dat from http://toshi.nofs.navy.mil/ser7/tai-utc.dat

c.f. http://www.lsc-group.phys.uwm.edu/~ballen/grasp-distribution/GRASP/doc/html/node311.html
-}

module Main where

import Data.Time
import Data.Time.Clock.TAI
import System.IO.Unsafe


{-# NOINLINE theLeapSecondTable #-}
theLeapSecondTable :: LeapSecondTable
theLeapSecondTable = parseTAIUTCDATFile $ unsafePerformIO $ 
  readFile "tai-utc.dat"


t1 :: UTCTime
t1 = UTCTime (fromGregorian 1980 1 1) 0

t1a :: AbsoluteTime
t1a = utcToTAITime theLeapSecondTable t1

t2 :: UTCTime
t2 = UTCTime (fromGregorian 2010 1 1) 0

t2a :: AbsoluteTime
t2a = utcToTAITime theLeapSecondTable t2

main :: IO ()
main = do
  let dt = diffUTCTime t2 t1 
  print dt
  let intDiffTime :: Integer
      intDiffTime = round dt
  putStrLn $ "leap seconds in UTC: " ++ show (intDiffTime `mod` 86400)

  let dta = diffAbsoluteTime t2a t1a  
  print dta
  putStrLn $ "leap seconds in TAI: " ++ show (round dta `mod` 86400)


{-

$ runhaskell test.hs
946771200s
leap seconds in UTC: 0
946771215s
leap seconds in TAI: 15



-}