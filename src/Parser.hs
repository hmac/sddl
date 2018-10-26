module Parser where

import           Control.Monad        (join, void)
import           Data.Void            (Void)
import           Lib                  (ColDef (ColDef), SqlType (..), Statement (AddColumn, AddForeignKey, CreateIndex, CreateTable, DropIndex, DropTable, MakeColumnNotNull, MakeColumnNull))

import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String


parseMigration :: String -> Either (ParseError Char Void) [Statement]
parseMigration = parse migration "input"

test :: IO ()
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
  string_ "--"
  _ <- takeWhileP (Just "ascii char") (/= '\n')
  pure []

addColumn :: Parser Statement
addColumn = do
  string_ "add_column"
  space1
  table <- word
  space1
  column <- word
  space1
  coltype <- parseSqlType
  pure $ AddColumn table column coltype

makeColumnNotNull :: Parser Statement
makeColumnNotNull = do
  string_ "set_column_not_null"
  space1
  table <- word
  space1
  column <- word
  pure $ MakeColumnNotNull table column

makeColumnNull :: Parser Statement
makeColumnNull = do
  string_ "set_column_null"
  space1
  table <- word
  space1
  column <- word
  pure $ MakeColumnNull table column

createIndex :: Parser Statement
createIndex = do
  string_ "add_index"
  space1
  table <- word
  space1
  cols <- between (char '(') (char ')') $ sepBy word (char ',' >> space)
  space1
  name <- word
  pure $ CreateIndex table cols name Nothing

dropIndex :: Parser Statement
dropIndex = do
  string_ "drop_index"
  space1
  name <- word
  pure $ DropIndex name

dropTable :: Parser Statement
dropTable = do
  string_ "drop_table"
  space1
  name <- word
  pure $ DropTable name

addForeignKey :: Parser Statement
addForeignKey = do
  string_ "add_foreign_key"
  srcTable <- space1 *> word
  srcCol <- space1 *> word
  tarTable <- space1 *> word
  tarCol <- space1 *> word
  pure $ AddForeignKey srcTable srcCol tarTable tarCol "todo"

createTable :: Parser Statement
createTable = do
  string_ "create_table"
  name <- space1 *> word
  prefix <- space1 *> (some upperChar)
  space1
  _ <- char '('
  space
  colDefs <- sepBy1 colDef (char ',' >> space)
  space
  _ <- char ')'
  pure $ CreateTable name prefix colDefs
    where colDef :: Parser ColDef
          colDef = do
            space
            col <- word
            space1
            coltype <- parseSqlType
            space
            isNull <- (string_ "null" >> pure True) <|> (string_ "not null" >> pure False)
            space
            pure $ ColDef col coltype isNull

parseSqlType :: Parser SqlType
parseSqlType = (string_ "text"      >> pure SText)
      <|> (string_ "timestamp" >> pure STimestamp)
      <|> (string_ "integer"   >> pure SInteger)
      <|> (string_ "boolean"   >> pure SBoolean)
      <|> (string_ "json"      >> pure SJson)

word :: Parser String
word = some (letterChar <|> char '_')

string_ :: String -> Parser ()
string_ = void . string
