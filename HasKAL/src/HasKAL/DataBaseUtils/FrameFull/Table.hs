

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}

module HasKAL.DataBaseUtils.FrameFull.Table
where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)


$(defineTable "framefull")
