{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Core                (Migration (..))
import qualified Data.ByteString     as B (getContents)
import qualified Data.Yaml           as Yaml
import           Options.Applicative
import qualified Reverse
import           System.Exit
import           ToSql               (toSql)
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
parseConfig = info parser $ fullDesc <> progDesc "sddl" <> header "sddl"
  where parser = Config <$> flag Forward Reverse (long "reverse" <> short 'r' <> help "Generate reverse migration")
