{-# LANGUAGE NamedFieldPuns #-}

module ToSql where

import           Core
import           Data.List (intercalate, intersperse)

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
  toSql (ColDef s t n) = let isNull = if n then "NULL" else "NOT NULL"
                          in unwords [s, toSql t, isNull]

instance ToSql Statement where
  toSql (AddColumn tbl col colType) =
    unwords ["ALTER TABLE", tbl, "ADD COLUMN", col, toSql colType]
  toSql (DropColumn tbl col) =
    unwords ["ALTER TABLE", tbl, "DROP COLUMN", col]
  toSql (CreateTable name p cols) =
    let idCol =
          "id character varying(32) DEFAULT " ++
          "public.gen_gc_id('" ++ p ++ "'::text, '" ++ name ++ "_id_seq'::text) NOT NULL"
        createSequence = "CREATE SEQUENCE public." ++ name ++ "_id_seq " ++
                     "START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;"
        colDefs = map (++ ",") $ idCol : map toSql cols
        createTable = multiline $ wrap ("CREATE TABLE " ++ name ++ " (") ")" colDefs
    in
      multiline [createSequence, createTable]
  toSql (MakeColumnNotNull table col) =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", col, "SET NOT NULL"]
  toSql (MakeColumnNull table col) =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", col, "SET NULL"]
  toSql (ChangeColumnDefault table col _ to) =
    unwords ["ALTER TABLE ONLY", table, "ALTER COLUMN", col, "SET DEFAULT", quote to]
  toSql (CreateIndex table cols name _mwhere) =
    unwords
      [ "CREATE INDEX CONCURRENTLY"
      , name
      , "ON"
      , table
      , "USING btree"
      , (paren . list) cols
      ]
  toSql (DropIndex name) = unwords ["DROP INDEX CONCURRENTLY", name]
  toSql (DropTable name) = unwords ["DROP TABLE", name]
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
  toSql (DropConstraint name) = unwords ["DROP CONSTRAINT", name]

instance ToSql Migration where
  toSql (Migration statements lockTimeout statementTimeout transaction) =
    let lt = "SET lock_timeout = " ++ show lockTimeout
        st = "SET statement_timeout = " ++ show statementTimeout
        commands = if transaction
                   then wrap "BEGIN" "COMMIT" $ lt : st : map toSql statements
                   else lt : st : map toSql statements
    in
      unlines $ map (++ ";") commands
