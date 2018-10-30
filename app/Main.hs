{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Arrow                as Arrow (left)
import           Control.Monad                ((>=>))
import qualified Data.ByteString              as B (ByteString, getContents,
                                                    putStr)
import           Data.Yaml                    (ToJSON, (.=))
import qualified Data.Yaml                    as Yaml
import           Options.Applicative
import           System.Exit                  (ExitCode (ExitFailure), exitWith)
import qualified Text.PrettyPrint.ANSI.Leijen as PrettyPrint (Doc, string)

import           Core                         (Migration (..))
import           Print                        (toSql)
import           Reverse
import           Validate

version :: String
version = "0.1.0.0"

main :: IO ()
main = do
  options <- execParser parseOptions
  input <- B.getContents
  case format options of
    FormatSql  ->
      case run (mode options) input of
        Left err        -> failWith err
        Right migration -> putStr (toSql migration)
    FormatYaml ->
      case run Forward input of
        Left err -> B.putStr $ Yaml.encode (E err)
        Right up ->
          let down = either (const Nothing) Just (run Reverse input)
          in B.putStr (Yaml.encode (Y up down))

failWith :: String -> IO ()
failWith err = putStrLn err >> exitWith (ExitFailure 1)

run :: Mode -> B.ByteString -> Either String Migration
run mode input =
  let generate = case mode of
                   Forward -> buildMigration
                   Reverse -> buildMigration >=> reverseMigration
  in do
    statements <- Arrow.left show (Yaml.decodeEither' input)
    generate statements

data Options = Options { mode :: Mode, format :: Format }
data Mode = Forward | Reverse
data Format = FormatSql | FormatYaml

-- This describes the our YAML output format
-- The first parameter is the up migration, the second is the down
data YamlOutput = Y Migration (Maybe Migration)
                | E String
instance ToJSON YamlOutput where
  toJSON (Y up down) = Yaml.object $
    case down of
      Nothing -> migrationToObject up
      Just Migration { statements } ->
        migrationToObject up <> [ "down" .= map toSql statements ]
    where
      migrationToObject Migration {statementTimeout, lockTimeout, transaction, statements} =
        [ "statement_timeout" .= statementTimeout
        , "lock_timeout" .= lockTimeout
        , "in_transaction" .= transaction
        , "up" .= map toSql statements
        ]
  toJSON (E err) = Yaml.object [ "error" .= err ]

parseOptions :: ParserInfo Options
parseOptions =
  info (parser <**> helper <**> versionOption) $
  fullDesc <> progDescDoc (Just description) <> header "sddl"
  where
    parser =
      Options <$>
      flag
        Forward
        Reverse
        (long "reverse" <> short 'r' <> help "Generate reverse migration") <*>
      option
        (maybeReader parseFormat)
        (long "format" <> short 'f' <> metavar "FORMAT" <> help "Output format. One of {sql, yaml}")
    versionOption :: Parser (a -> a)
    versionOption = infoOption version (long "version" <> help "Show version")

parseFormat :: String -> Maybe Format
parseFormat =
  \case
    "sql" -> Just FormatSql
    "yaml" -> Just FormatYaml
    _ -> Nothing

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
  , "- { tag: drop_constraint, name: constraint name (e.g. a foreign key name) }"
  , "- tag: create_table"
  , "  table: table name"
  , "  prefix: the ID prefix (e.g. 'PM')"
  , "  definition: (an array of colun definitions, like this)"
  , "  - column: column name"
  , "  - type: SQL type"
  , "  - null: (boolean) whether the column is nullable"
  , "- tag: add_foreign_key"
  , "  source_table: source table name"
  , "  source_column: source column name"
  , "  target_tabl: target table name"
  , "  target_column: target column name"
  , "  name: name of the foreign key"
  , "- tag: validate_foreign_key"
  , "  table: table containing the foreign key"
  , "  name: name of the foreign key"
  , ""
  , "SQL type is one of {text, timestamp, integer, boolean, json}"
  ]
