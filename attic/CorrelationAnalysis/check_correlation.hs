import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)

import System.Environment (getArgs)

import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod

main = do

 let data1 = NLA.fromList [1..10] :: NLA.Vector Double
     data2 = NLA.fromList [5,2,3,1,57,4,2,4,5,7] :: NLA.Vector Double
     data3 = NLA.fromList [2,3,1,57,4,2,4,5,7,8] :: NLA.Vector Double

 let rValue1 = takeCorrelationV Peason data1 data2 2
 print rValue1

 let rValue2 = takeCorrelationV Peason data2 data1 2
 print rValue2

 let rValue3 = takeCorrelationV Peason data2 data3 2
 print rValue3

 let rValue4 = takeCorrelationV Peason data3 data2 2
 print rValue4
