module Parser where

import           Control.Monad        (join)
import           Data.Void            (Void)
import           Lib

import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String


parseMigration :: String -> Either (ParseError Char Void) [Statement]
parseMigration input = parse migration "input" input

test =
  let tests =
          [ "add_column apps created_at timestamp"
          , "set_column_null apps created_at"
          , "set_column_null apps created_at"
          , "add_index apps (created_at) apps_created_at_index"
          , "drop_index apps_created_at_index"
          , "drop_table apps"
          , "add_foreign_key apps organisation_id organisations id"
          , "create_table apps AP ( organisation_id text not null, name text not null, is_disabled boolean null)"
          ]
  in mapM_ (parseTest migration) tests

migration :: Parser [Statement]
migration = do
  statements <- many ((comment <|> statement) <* newline)
  space
  eof
  pure (join statements)

statement :: Parser [Statement]
statement = do
  s <- addColumn <|> createTable <|> makeColumnNotNull <|> makeColumnNull <|> createIndex <|> dropIndex <|> dropTable <|> addForeignKey
  pure [s]

comment :: Parser [Statement]
comment = do
  string "--"
  takeWhileP (Just "ascii char") (/= '\n')
  pure []

addColumn :: Parser Statement
addColumn = do
  string "add_column"
  space1
  table <- word
  space1
  column <- word
  space1
  coltype <- parseSqlType
  pure $ AddColumn table column coltype

makeColumnNotNull :: Parser Statement
makeColumnNotNull = do
  string "set_column_not_null"
  space1
  table <- word
  space1
  column <- word
  pure $ MakeColumnNotNull table column

makeColumnNull :: Parser Statement
makeColumnNull = do
  string "set_column_null"
  space1
  table <- word
  space1
  column <- word
  pure $ MakeColumnNull table column

createIndex :: Parser Statement
createIndex = do
  string "add_index"
  space1
  table <- word
  space1
  cols <- between (char '(') (char ')') $ sepBy word (char ',' >> space)
  space1
  name <- word
  pure $ CreateIndex table cols name Nothing

dropIndex :: Parser Statement
dropIndex = do
  string "drop_index"
  space1
  name <- word
  pure $ DropIndex name

dropTable :: Parser Statement
dropTable = do
  string "drop_table"
  space1
  name <- word
  pure $ DropTable name

addForeignKey :: Parser Statement
addForeignKey = do
  string "add_foreign_key"
  srcTable <- space1 *> word
  srcCol <- space1 *> word
  tarTable <- space1 *> word
  tarCol <- space1 *> word
  pure $ AddForeignKey srcTable srcCol tarTable tarCol "todo"

createTable :: Parser Statement
createTable = do
  string "create_table"
  name <- space1 *> word
  prefix <- space1 *> (some upperChar)
  space1
  char '('
  space
  colDefs <- sepBy1 colDef (char ',' >> space)
  space
  char ')'
  pure $ CreateTable name prefix colDefs
    where colDef :: Parser ColDef
          colDef = do
            space
            col <- word
            space1
            coltype <- parseSqlType
            space
            null <- (string "null" >> pure True) <|> (string "not null" >> pure False)
            space
            pure $ ColDef col coltype null

parseSqlType :: Parser SqlType
parseSqlType = (string "text"      >> pure SText)
      <|> (string "timestamp" >> pure STimestamp)
      <|> (string "integer"   >> pure SInteger)
      <|> (string "boolean"   >> pure SBoolean)
      <|> (string "json"      >> pure SJson)

word :: Parser String
word = some (letterChar <|> char '_')
