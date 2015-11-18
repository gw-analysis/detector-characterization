

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HasKAL.DataBaseUtils.XEndEnv.Table 
where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)


$(defineTable "xendenv")



