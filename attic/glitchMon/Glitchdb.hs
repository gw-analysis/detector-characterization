{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GlitchMon.Glitchdb where

import Prelude hiding (id)
import HasKAL.DataBaseUtils.DataSource (defineTable)
import Data.Int (Int32, Int64)



$(defineTable [ ("CHAR",       [t| String |])
              , ("VARCHAR",    [t| String |])
              , ("TINYTEXT",   [t| String |])
              , ("TEXT",       [t| String |])
              , ("MEDIUMTEXT", [t| String |])
              , ("LONGTEXT",   [t| String |])
              , ("TINYINT",    [t| Int32 |])
              , ("SMALLINT",   [t| Int32 |])
              , ("INT",        [t| Int32 |])
              , ("INTEGER",    [t| Int32 |])
              , ("BIGINT",     [t| Int64 |])
              , ("DOUBLE",     [t| Double |])
              ] "KAGRA" "glitchdb" [])




