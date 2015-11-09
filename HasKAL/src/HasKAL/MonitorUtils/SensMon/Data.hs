


module HasKAL.MonitorUtils.SensMon.Data
where


data SensParam = SensParam
       { histmin :: Double
       , histmax :: Double
       , ndiv :: Int
       , binInterval :: Double
       , binlist :: [Double]
       }

updateSensParam'histmin :: SensParam -> Double -> SensParam
updateSensParam'histmin p x = p {histmin = x}

updateSensParam'histmax :: SensParam -> Double -> SensParam
updateSensParam'histmax p x = p {histmax = x}

updateSensParam'ndiv :: SensParam -> Int -> SensParam
updateSensParam'ndiv p n = p {ndiv = n}

updateSensParam'binInterval :: SensParam -> Double -> SensParam
updateSensParam'binInterval p bi = p {binInterval = bi}

updateSensParam'binlist :: SensParam -> [Double] -> SensParam
updateSensParam'binlist p bl = p {binlist = bl}





