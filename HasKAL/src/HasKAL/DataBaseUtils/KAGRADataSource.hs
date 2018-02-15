{-# LANGUAGE TemplateHaskell #-}

module HasKAL.DataBaseUtils.KAGRADataSource
( connect
, convTypes
, defineTable
, defineTableforSqlite3
) where

import Data.Int (Int32, Int64)
import Database.HDBC.MySQL
import Database.HDBC.Query.TH (defineTableFromDB')
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.MySQL (driverMySQL)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (connectSqlite3)
--import Database.Record.TH (derivingShow)
import Database.Relational.Config (defaultConfig
                                           , normalizedTableName)
import Language.Haskell.TH (Q, Dec, TypeQ)


connect :: IO Connection
connect = connectMySQL defaultMySQLConnectInfo {mysqlDatabase = "KAGRA"}


convTypes :: [(String, TypeQ)]
convTypes = [ ("CHAR",       [t| String |])
            , ("VARCHAR",    [t| String |])
            , ("TINYTEXT",   [t| String |])
            , ("TEXT",       [t| String |])
            , ("MEDIUMTEXT", [t| String |])
            , ("LONGTEXT",   [t| String |])
            , ("TINYINT",    [t| Int32 |])
            , ("SMALLINT",   [t| Int32 |])
            , ("INT",        [t| Int32 |])
            , ("MEDIUMINT",  [t| Int32 |])
            , ("INTEGER",    [t| Int32 |])
            , ("BIGINT",     [t| Int64 |])
            , ("DOUBLE",     [t| Double |])
            ]


defineTable :: String -> Q [Dec]
defineTable tableName =
  defineTableFromDB'
    connect
    (defaultConfig { normalizedTableName = False })
    (driverMySQL { typeMap = convTypes })
    "KAGRA"
    tableName
    [''Show]


defineTableforSqlite3 :: String -> String -> Q [Dec]
defineTableforSqlite3 dbfilename tableName =
  defineTableFromDB'
    (connectSqlite3 dbfilename)
    (defaultConfig { normalizedTableName = False })
    (driverSQLite3 { typeMap = convTypes })
    "main" -- schema name, ignored by SQLite
    tableName
    [''Show]
