

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module HasKAL.DataBaseUtils.XEndEnv.Table
where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)


$(defineTable "xendenv")
