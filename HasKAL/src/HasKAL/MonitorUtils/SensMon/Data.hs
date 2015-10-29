


module HasKAL.MonitorUtils.SensMon.Data
where


data SensParam = SensParam
       { histmin :: Double
       , histmax :: Double
       , ndiv :: Int
       , binInterval :: Double
       , binlist :: [Double]
       }

