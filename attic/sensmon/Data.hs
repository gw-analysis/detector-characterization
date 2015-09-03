


module Data
where


data SensParam = SensParam
       { histmin :: Double
       , histmax :: Double
       , binInterval :: Double
       , binlist :: [Double]
       }

