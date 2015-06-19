{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HasKAL.DataBaseUtils.Framedb where

import Prelude hiding (id)
import HasKAL.DataBaseUtils.DataSource (defineTable)

$(defineTable [] "KAGRA" "framedb" [])


