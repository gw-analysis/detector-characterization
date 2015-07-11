{- |
Module      : TaiUtc
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

Reference:
  http://maia.usno.navy.mil
  http://maia.usno.navy.mil/ser7/tai-utc.dat
-}{-
  * Last Modified: 2015/07/10 13:40:04
-}


module HasKAL.TimeUtils.TaiUtc (
  taiTable
) where

taiTable :: String
taiTable = unlines [
  " 1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S",
  " 1961 AUG  1 =JD 2437512.5  TAI-UTC=   1.3728180 S + (MJD - 37300.) X 0.001296 S",
  " 1962 JAN  1 =JD 2437665.5  TAI-UTC=   1.8458580 S + (MJD - 37665.) X 0.0011232S",
  " 1963 NOV  1 =JD 2438334.5  TAI-UTC=   1.9458580 S + (MJD - 37665.) X 0.0011232S",
  " 1964 JAN  1 =JD 2438395.5  TAI-UTC=   3.2401300 S + (MJD - 38761.) X 0.001296 S",
  " 1964 APR  1 =JD 2438486.5  TAI-UTC=   3.3401300 S + (MJD - 38761.) X 0.001296 S",
  " 1964 SEP  1 =JD 2438639.5  TAI-UTC=   3.4401300 S + (MJD - 38761.) X 0.001296 S",
  " 1965 JAN  1 =JD 2438761.5  TAI-UTC=   3.5401300 S + (MJD - 38761.) X 0.001296 S",
  " 1965 MAR  1 =JD 2438820.5  TAI-UTC=   3.6401300 S + (MJD - 38761.) X 0.001296 S",
  " 1965 JUL  1 =JD 2438942.5  TAI-UTC=   3.7401300 S + (MJD - 38761.) X 0.001296 S",
  " 1965 SEP  1 =JD 2439004.5  TAI-UTC=   3.8401300 S + (MJD - 38761.) X 0.001296 S",
  " 1966 JAN  1 =JD 2439126.5  TAI-UTC=   4.3131700 S + (MJD - 39126.) X 0.002592 S",
  " 1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S",
  " 1972 JAN  1 =JD 2441317.5  TAI-UTC=  10.0       S + (MJD - 41317.) X 0.0      S",
  " 1972 JUL  1 =JD 2441499.5  TAI-UTC=  11.0       S + (MJD - 41317.) X 0.0      S",
  " 1973 JAN  1 =JD 2441683.5  TAI-UTC=  12.0       S + (MJD - 41317.) X 0.0      S",
  " 1974 JAN  1 =JD 2442048.5  TAI-UTC=  13.0       S + (MJD - 41317.) X 0.0      S",
  " 1975 JAN  1 =JD 2442413.5  TAI-UTC=  14.0       S + (MJD - 41317.) X 0.0      S",
  " 1976 JAN  1 =JD 2442778.5  TAI-UTC=  15.0       S + (MJD - 41317.) X 0.0      S",
  " 1977 JAN  1 =JD 2443144.5  TAI-UTC=  16.0       S + (MJD - 41317.) X 0.0      S",
  " 1978 JAN  1 =JD 2443509.5  TAI-UTC=  17.0       S + (MJD - 41317.) X 0.0      S",
  " 1979 JAN  1 =JD 2443874.5  TAI-UTC=  18.0       S + (MJD - 41317.) X 0.0      S",
  " 1980 JAN  1 =JD 2444239.5  TAI-UTC=  19.0       S + (MJD - 41317.) X 0.0      S",
  " 1981 JUL  1 =JD 2444786.5  TAI-UTC=  20.0       S + (MJD - 41317.) X 0.0      S",
  " 1982 JUL  1 =JD 2445151.5  TAI-UTC=  21.0       S + (MJD - 41317.) X 0.0      S",
  " 1983 JUL  1 =JD 2445516.5  TAI-UTC=  22.0       S + (MJD - 41317.) X 0.0      S",
  " 1985 JUL  1 =JD 2446247.5  TAI-UTC=  23.0       S + (MJD - 41317.) X 0.0      S",
  " 1988 JAN  1 =JD 2447161.5  TAI-UTC=  24.0       S + (MJD - 41317.) X 0.0      S",
  " 1990 JAN  1 =JD 2447892.5  TAI-UTC=  25.0       S + (MJD - 41317.) X 0.0      S",
  " 1991 JAN  1 =JD 2448257.5  TAI-UTC=  26.0       S + (MJD - 41317.) X 0.0      S",
  " 1992 JUL  1 =JD 2448804.5  TAI-UTC=  27.0       S + (MJD - 41317.) X 0.0      S",
  " 1993 JUL  1 =JD 2449169.5  TAI-UTC=  28.0       S + (MJD - 41317.) X 0.0      S",
  " 1994 JUL  1 =JD 2449534.5  TAI-UTC=  29.0       S + (MJD - 41317.) X 0.0      S",
  " 1996 JAN  1 =JD 2450083.5  TAI-UTC=  30.0       S + (MJD - 41317.) X 0.0      S",
  " 1997 JUL  1 =JD 2450630.5  TAI-UTC=  31.0       S + (MJD - 41317.) X 0.0      S",
  " 1999 JAN  1 =JD 2451179.5  TAI-UTC=  32.0       S + (MJD - 41317.) X 0.0      S",
  " 2006 JAN  1 =JD 2453736.5  TAI-UTC=  33.0       S + (MJD - 41317.) X 0.0      S",
  " 2009 JAN  1 =JD 2454832.5  TAI-UTC=  34.0       S + (MJD - 41317.) X 0.0      S",
  " 2012 JUL  1 =JD 2456109.5  TAI-UTC=  35.0       S + (MJD - 41317.) X 0.0      S",
  " 2015 JUL  1 =JD 2457204.5  TAI-UTC=  36.0       S + (MJD - 41317.) X 0.0      S"
  ]