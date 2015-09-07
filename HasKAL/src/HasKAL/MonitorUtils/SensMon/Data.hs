


module HasKAL.MonitorUtils.SensMon.Data
where


data SensParam = SensParam
       { histmin :: Double
       , histmax :: Double
       , binInterval :: Double
       , binlist :: [Double]
       }

