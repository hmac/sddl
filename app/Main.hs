{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Core                         (Migration (..))
import qualified Data.ByteString              as B (getContents)
import qualified Data.Yaml                    as Yaml
import           Options.Applicative
import qualified Reverse
import           System.Exit
import qualified Text.PrettyPrint.ANSI.Leijen as PrettyPrint (Doc, string)
import           ToSql                        (toSql)
import           Validator

main :: IO ()
main = do
  config <- execParser parseConfig
  input <- B.getContents
  case Yaml.decodeEither' input of
    Left (Yaml.AesonException e) -> putStrLn e
    Left e -> print e
    Right statements ->
      let lockTimeout = calculateLockTimeout statements
          statementTimeout = calculateStatementTimeout statements
          transaction = determineTransaction statements
          migration = Migration statements lockTimeout statementTimeout transaction
      in
        case validate migration of
          []   ->
            case mode config of
              Forward -> putStrLn (toSql migration)
              Reverse -> case Reverse.reverse migration of
                           Nothing -> putStrLn "ERROR: cannot reverse migration"
                           Just m  -> putStrLn (toSql m)
          errs -> putStrLn ("ERROR: " ++ unlines errs) >> exitWith (ExitFailure 1)

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
  , "The supported actions are (in YAML):"
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
