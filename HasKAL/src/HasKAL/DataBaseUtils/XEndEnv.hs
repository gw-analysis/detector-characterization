

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HasKAL.DataBaseUtils.XEndEnv 
where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)


$(defineTable "xendenv")



