{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.ByteString as B (getContents)
import qualified Data.Yaml       as Yaml
import           Lib             (Migration (..))
import           System.Exit
import           ToSql           (toSql)
import           Validator

main :: IO ()
main = do
  input <- B.getContents
  statements <- Yaml.decodeThrow input
  let lockTimeout = calculateLockTimeout statements
      statementTimeout = calculateStatementTimeout statements
      transaction = determineTransaction statements
      migration = Migration statements lockTimeout statementTimeout transaction
  case validate migration of
    []   -> putStrLn (toSql migration)
    errs -> putStrLn ("ERROR: " ++ unlines errs) >> exitWith (ExitFailure 1)
