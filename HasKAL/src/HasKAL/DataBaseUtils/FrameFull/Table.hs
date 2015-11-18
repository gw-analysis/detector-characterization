

{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module HasKAL.DataBaseUtils.FrameFull.Table 
where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)


$(defineTable "framefull")



