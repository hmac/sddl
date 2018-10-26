{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.ByteString as B (getContents)
import qualified Data.Yaml       as Yaml
import           Lib             (Migration (..))
import qualified Reverse
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
    []   -> do
      putStrLn (toSql migration)
      case Reverse.reverse migration of
        Nothing -> putStrLn "ERROR: cannot reverse migration"
        Just m  -> putStrLn (toSql m)
    errs -> putStrLn ("ERROR: " ++ unlines errs) >> exitWith (ExitFailure 1)
