{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glitchdb where

import Prelude hiding (id)
import HasKAL.DataBaseUtils.DataSource (defineTable)
import Data.Int (Int64)



$(defineTable [ ("CHAR",       [t| String |])
              , ("VARCHAR",    [t| String |])
              , ("TINYTEXT",   [t| String |])
              , ("TEXT",       [t| String |])
              , ("MEDIUMTEXT", [t| String |])
              , ("LONGTEXT",   [t| String |])
              , ("TINYINT",    [t| Int |])
              , ("SMALLINT",   [t| Int |])
              , ("INT",        [t| Int |])
              , ("INTEGER",    [t| Int |])
              , ("BIGINT",     [t| Int64 |])
              , ("DOUBLE",     [t| Double |])
              ] "KAGRA" "glitchdb" [])




