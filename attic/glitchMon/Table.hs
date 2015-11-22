{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GlitchMon.Table where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)



$(defineTable "glitchtbl")




