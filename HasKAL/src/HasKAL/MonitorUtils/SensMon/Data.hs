


module HasKAL.MonitorUtils.SensMon.Data
where


data SensParam = SensParam
       { histmin :: Double
       , histmax :: Double
       , ndiv :: Int
       , binInterval :: Double
       , binlist :: [Double]
       }

updateSensParam'histmin :: Double -> SensParam
updateSensParam'histmin x = SensParam {histmin = x}

updateSensParam'histmax :: Double -> SensParam
updateSensParam'histmax x = SensParam {histmax = x}

updateSensParam'ndiv :: Int -> SensParam
updateSensParam'ndiv n = SensParam {ndiv = n}

updateSensParam'inInterval :: Double -> SensParam
updateSensParam'inInterval bi = SensParam {binInterval = bi}

updateSensParam'binlist :: [Double] -> SensParam
updateSensParam'binlist bl = SensParam {binlist = bl}





