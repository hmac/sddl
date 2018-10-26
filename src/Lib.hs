{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib where

import           Control.Monad   (mzero)
import           Data.Aeson      (FromJSON, Options (..), camelTo2,
                                  defaultOptions, genericParseJSON, parseJSON,
                                  withText)
import           Data.List       (intercalate, intersperse)
import           Data.Maybe      (fromMaybe)
import           GHC.Generics

import           Numeric.Natural

data SqlType = SText | STimestamp | SInteger | SBoolean | SJson
  deriving (Eq, Show, Generic)

instance FromJSON SqlType where
  parseJSON = withText "a SQL type" $ \t ->
    case t of
      "text"      -> pure SText
      "timestamp" -> pure STimestamp
      "integer"   -> pure SInteger
      "boolean"   -> pure SBoolean
      "json"      -> pure SJson
      s           -> fail $ "Invalid SQL type " ++ show s

data Statement =
    AddColumn { table :: String, column :: String, sqlType :: SqlType }
  | CreateTable { name :: String, prefix :: String, definition :: [ColDef] }
  | MakeColumnNotNull { table :: String, column :: String }
  | MakeColumnNull { table :: String, column :: String }
  | CreateIndex { table :: String, columns :: [String], name :: String, partial :: Maybe String }
  | DropIndex { name :: String }
  | DropTable { name :: String }
  | AddForeignKey { sourceTable  :: String
                  , sourceColumn :: String
                  , targetTable  :: String
                  , targetColumn :: String
                  , name         :: String
                  }
  deriving (Eq, Show, Generic)

instance FromJSON Statement where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_'
                                              , fieldLabelModifier = camelTo2 '_' . specialCaseSqlType }

specialCaseSqlType :: String -> String
specialCaseSqlType "sqlType" = "type"
specialCaseSqlType x         = x

data ColDef =
  ColDef { name :: String, sqlType :: SqlType, null :: Bool }
  deriving (Eq, Show, Generic)

instance FromJSON ColDef where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . specialCaseSqlType }

data Migration = Migration { statements :: [Statement], lockTimeout :: Natural, statementTimeout :: Natural }
  deriving (Eq, Show, Generic)

instance FromJSON Migration

newtype LockTimeout = LockTimeout Int deriving (Eq, Show, Generic)
instance FromJSON LockTimeout

newtype StatementTimeout = StatementTimeout Int deriving (Eq, Show, Generic)
instance FromJSON StatementTimeout

class ToSql a where
  toSql :: a -> String

instance ToSql SqlType where
  toSql SText      = "text"
  toSql STimestamp = "timestamp without time zone"
  toSql SInteger   = "integer"
  toSql SBoolean   = "boolean"
  toSql SJson      = "jsonb"

instance ToSql ColDef where
  toSql (ColDef s t n) = let null = if n then "NULL" else "NOT NULL"
                          in unwords [s, toSql t, null]

instance ToSql Statement where
  toSql (AddColumn table col colType) =
    unwords ["ALTER TABLE", table, "ADD COLUMN", col, toSql colType]
  toSql (CreateTable name p cols) =
    let idCol =
          "id character varying(32) DEFAULT " ++
          "public.gen_gc_id('" ++ p ++ "'::text, '" ++ name ++ "_id_seq'::text, 22) NOT NULL"
        create_seq = "CREATE SEQUENCE public." ++ name ++ "_id_seq " ++
                     "START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;"
    in unlines
        [create_seq
       , unwords ["CREATE TABLE", name, "(\n", multilist (idCol : map toSql cols), "\n)"]
        ]
  toSql (MakeColumnNotNull table col) =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", col, "SET NOT NULL"]
  toSql (MakeColumnNull table col) =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", col, "SET NULL"]
  toSql (CreateIndex table cols name mwhere) =
    let whereC = fromMaybe "" mwhere
    in unwords
         [ "CREATE INDEX CONCURRENTLY"
         , name
         , "ON"
         , table
         , "USING btree"
         , (paren . list) cols
         ]
  toSql (DropIndex name) = unwords ["DROP INDEX CONCURRENTLY", name]
  toSql (AddForeignKey srcTable srcCol tarTable tarCol name) =
    unwords
      [ "ALTER TABLE ONLY"
      , srcTable
      , "ADD CONSTRAINT"
      , name
      , "FOREIGN KEY"
      , paren srcCol
      , "REFERENCES"
      , tarTable ++ paren tarCol
      ]

paren s = "(" ++ s ++ ")"
list xs = unwords (intersperse "," xs)
multilist = intercalate ",\n"

instance ToSql Migration where
  toSql (Migration statements lockTimeout statementTimeout) =
    let lt = "SET lock_timeout = " ++ show lockTimeout
        st = "SET statement_timeout = " ++ show statementTimeout
    in
      unlines $ map (++ ";") $ lt : st : map toSql statements
