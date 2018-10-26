module Main where

import           Lib
import           Parser
import           Validator

import           System.Exit

import qualified Data.ByteString as B (getContents)
import           Data.Char       (isUpper, toLower)
import           Data.Text       (Text)
import qualified Data.Text.IO    as T (getContents)

import           GHC.Generics

import qualified Data.Yaml       as Yaml
import           Numeric.Natural

main :: IO ()
main = do
  input <- B.getContents
  statements <- Yaml.decodeThrow input
  let lockTimeout = calculateLockTimeout statements
      statementTimeout = calculateStatementTimeout statements
      migration = Migration statements lockTimeout statementTimeout
  case validate migration of
    Left e          -> putStrLn ("ERROR: " ++ e) >> exitWith (ExitFailure 1)
    Right migration -> putStrLn (toSql migration)
