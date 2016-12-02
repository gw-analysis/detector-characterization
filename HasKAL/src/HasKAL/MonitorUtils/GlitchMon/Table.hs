{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HasKAL.MonitorUtils.GlitchMon.Table where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)



$(defineTable "glitchtbl")




