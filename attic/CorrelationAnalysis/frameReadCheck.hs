-- test code of HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
-- read 1 frame data and for each channel data, calculate correlation value.


-- usage: Neew one argument (Frame File)
-- check_correlation /data/ligo/archive/frames/S6/L1/LLO/L-L1_RDS_R_L1-9592/L-L1_RDS_R_L1-959200000-64.gwf

-- First you must execute below command
-- setupChannelList.sh


import Numeric.LinearAlgebra  --subVector
import Numeric --showFFloat
import Control.Monad -- forM
import System.Environment -- getArgs
--import System.Cmd -- system
import System.Process -- system
import Data.List.Split -- splitOn

import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.FrameUtils.FrameUtils -- read Frame file


main = do

 let fs = "128"::String


 [frameFileName] <- getArgs
 print $ frameFileName
 let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" frameFileName :: Integer
 print getGpsTime
 
 -- read Channel List
 --channelList <- getChannelList frameFileName
 --print channelList

 let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
 let fs = 2048 :: Integer

 