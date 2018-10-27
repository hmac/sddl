{-# LANGUAGE NamedFieldPuns #-}

module Validate (buildMigration) where

import           Core
import           Data.List       (intercalate)
import           Data.Maybe      (mapMaybe)
import           Numeric.Natural

type Validation = Migration -> Maybe String

-- buildMigration takes a list of statements and produces either a valid
-- migration or a set of errors
buildMigration :: [Statement] -> Either String Migration
buildMigration s =
  let m = Migration { statements = s
                    , lockTimeout = calculateLockTimeout s
                    , statementTimeout = calculateStatementTimeout s
                    , transaction = determineTransaction s
                    }
   in case validate m of
        []   -> Right m
        errs -> Left (intercalate ", " errs)

calculateLockTimeout :: [Statement] -> Natural
calculateLockTimeout statements =
  if includesIndex statements
  then 900000
  else 750

calculateStatementTimeout :: [Statement] -> Natural
calculateStatementTimeout statements =
  if includesIndex statements
  then 900000
  else 1500

-- Every migration should occur in a transaction, except for index creates and drops
determineTransaction :: [Statement] -> Bool
determineTransaction = not . includesIndex

validate :: Migration -> [String]
validate migration = mapMaybe (\v -> v migration) validations

validations :: [Validation]
validations = [indexCreationIsolation]

indexCreationIsolation :: Migration -> Maybe String
indexCreationIsolation Migration { statements } =
  if includesIndex statements && length statements > 1
  then Just "Perform index creation in its own migration"
  else Nothing

-- Does the list of statements include an index create or an index drop?
includesIndex :: [Statement] -> Bool
includesIndex = any isIndexCreationOrDrop
  where isIndexCreationOrDrop CreateIndex {} = True
        isIndexCreationOrDrop DropIndex {}   = True
        isIndexCreationOrDrop _              = False
