{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module ToSql where

import           Core
import           Data.List (intercalate, intersperse)
import           Prelude   hiding (null)

-- Utilities
paren :: String -> String
paren s = "(" ++ s ++ ")"

list :: [String] -> String
list xs = unwords (intersperse "," xs)

multiline :: [String] -> String
multiline = intercalate "\n"

quote :: String -> String
quote s = "'" ++ s ++ "'"

wrap :: String -> String -> [String] -> [String]
wrap begin end mid = begin : (mid ++ [end])

class ToSql a where
  toSql :: a -> String

instance ToSql SqlType where
  toSql SText      = "text"
  toSql STimestamp = "timestamp without time zone"
  toSql SInteger   = "integer"
  toSql SBoolean   = "boolean"
  toSql SJson      = "jsonb"

instance ToSql ColDef where
  toSql ColDef { name, type_, null } =
    let isNull = if null then "NULL" else "NOT NULL"
    in unwords [name, toSql type_, isNull]

instance ToSql Statement where
  toSql AddColumn { table, column, type_ } =
    unwords ["ALTER TABLE", table, "ADD COLUMN", column, toSql type_]
  toSql DropColumn { table, column } =
    unwords ["ALTER TABLE", table, "DROP COLUMN", column]
  toSql CreateTable { table, prefix, definition } =
    let idCol =
          "id character varying(32) DEFAULT " ++
          "public.gen_gc_id('" ++ prefix ++ "'::text, '" ++ table ++ "_id_seq'::text) NOT NULL"
        createSequence = "CREATE SEQUENCE public." ++ table ++ "_id_seq " ++
                     "START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;"
        colDefs = map (++ ",") $ idCol : map toSql definition
        createTable = multiline $ wrap ("CREATE TABLE " ++ table ++ " (") ")" colDefs
    in
      multiline [createSequence, createTable]
  toSql MakeColumnNotNull { table, column } =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", column, "SET NOT NULL"]
  toSql MakeColumnNull { table, column } =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", column, "SET NULL"]
  toSql ChangeColumnDefault { table, column, to } =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", column, "SET DEFAULT", quote to]
  toSql DropColumnDefault { table, column } =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", column, "DROP DEFAULT"]
  toSql CreateIndex { table, columns, name } =
    unwords
      [ "CREATE INDEX CONCURRENTLY"
      , name
      , "ON"
      , table
      , "USING btree"
      , (paren . list) columns
      ]
  toSql DropIndex { name } = unwords ["DROP INDEX CONCURRENTLY", name]
  toSql DropTable { table } = unwords ["DROP TABLE", table]
  toSql AddForeignKey { sourceTable, sourceColumn, targetTable, targetColumn, name } =
    unwords
      [ "ALTER TABLE ONLY"
      , sourceTable
      , "ADD CONSTRAINT"
      , name
      , "FOREIGN KEY"
      , paren sourceColumn
      , "REFERENCES"
      , targetTable ++ paren targetColumn
      ]
  toSql DropConstraint { name } = unwords ["DROP CONSTRAINT", name]

instance ToSql Migration where
  toSql Migration { statements, lockTimeout, statementTimeout, transaction } =
    let lt = "SET lock_timeout = " ++ show lockTimeout
        st = "SET statement_timeout = " ++ show statementTimeout
        commands = if transaction
                   then wrap "BEGIN" "COMMIT" $ lt : st : map toSql statements
                   else lt : st : map toSql statements
    in
      unlines $ map (++ ";") commands
