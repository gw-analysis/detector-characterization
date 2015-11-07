

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HasKAL.DataBaseUtils.XEndEnv 
where

import KAGRADataSource (defineTable)


$(defineTable "xendenv")



