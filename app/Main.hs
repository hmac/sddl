{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Control.Arrow                as Arrow (left)
import           Control.Monad                ((>=>))
import qualified Data.ByteString              as B (ByteString, getContents)
import qualified Data.Yaml                    as Yaml
import           Options.Applicative
import           System.Exit                  (ExitCode (ExitFailure), exitWith)
import qualified Text.PrettyPrint.ANSI.Leijen as PrettyPrint (Doc, string)

import           Core                         (Migration)
import           Print                        (toSql)
import           Reverse
import           Validate

main :: IO ()
main = do
  config <- execParser parseConfig
  input <- B.getContents
  case run (mode config) input of
    Left err        -> putStrLn err >> exitWith (ExitFailure 1)
    Right migration -> putStrLn (toSql migration)

run :: Mode -> B.ByteString -> Either String Migration
run mode input =
  let generate = case mode of
                   Forward -> buildMigration
                   Reverse -> buildMigration >=> reverseMigration
  in do
    statements <- Arrow.left show (Yaml.decodeEither' input)
    generate statements

newtype Config = Config { mode :: Mode } deriving (Eq, Show)
data Mode = Forward | Reverse deriving (Eq, Show)

parseConfig :: ParserInfo Config
parseConfig = info (parser <**> helper) $ fullDesc <> progDescDoc (Just description) <> header "sddl"
  where parser = Config <$> flag Forward Reverse (long "reverse" <> short 'r' <> help "Generate reverse migration")

description :: PrettyPrint.Doc
description = PrettyPrint.string $ unlines
  ["sddl: A tool to generate safe DDL"
  , ""
  , "Pipe YAML to this tool and it will produce SQL migrations."
  , "The supported actions are (in YAML syntax):"
  , ""
  , "- { tag: add_column, table: table name, column: column name, type: SQL type }"
  , "- { tag: drop_column, table: table name, column: column name }"
  , "- { tag: make_column_null, table: table name, column: column name }"
  , "- { tag: make_column_not_null, table: table name, column: column name }"
  , "- tag: change_column_default"
  , "  table: table name"
  , "  column: column name"
  , "  from: current SQL type"
  , "  to: new SQL type"
  , "- { tag: drop_column_default, table: table name, column: column name, from: current SQL type }"
  , "- { tag: create_index, table: table name, columns: [array of column names], name: index name }"
  , "- { tag: drop_index, name: index name }"
  , "- { tag: drop_table, table: table name }"
  , "- tag: create_table"
  , "  table: table name"
  , "  prefix: the ID prefix (e.g. 'PM')"
  , "  definition: (an array of colun definitions, like this)"
  , "  - name: column name"
  , "  - type: SQL type"
  , "  - null: (boolean) whether the column is nullable"
  , ""
  , "SQL type is one of {text timestamp integer boolean json}"
  ]
