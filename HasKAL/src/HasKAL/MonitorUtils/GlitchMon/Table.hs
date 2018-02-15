{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module HasKAL.MonitorUtils.GlitchMon.Table where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)



$(defineTable "glitchtbl")
