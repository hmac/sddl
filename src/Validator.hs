{-# LANGUAGE NamedFieldPuns #-}

module Validator
  ( validate
  , calculateLockTimeout
  , calculateStatementTimeout
  , determineTransaction
  ) where

import           Lib
import           Numeric.Natural

import           Data.Maybe      (mapMaybe)

type Validation = Migration -> Maybe String

calculateLockTimeout :: [Statement] -> Natural
calculateLockTimeout statements = if includesIndexCreation statements then 900000 else 750

calculateStatementTimeout :: [Statement] -> Natural
calculateStatementTimeout statements = if includesIndexCreation statements then 900000 else 1500

-- Every migration should occur in a transaction, except for index creation
determineTransaction :: [Statement] -> Bool
determineTransaction [CreateIndex {}] = False
determineTransaction _                = True

validate :: Migration -> [String]
validate migration =
  mapMaybe (\v -> v migration) validations

validations :: [Validation]
validations = [indexCreationIsolation]

indexCreationIsolation :: Migration -> Maybe String
indexCreationIsolation Migration { statements } =
  if includesIndexCreation statements && length statements > 1
  then Just "Perform index creation in its own migration"
  else Nothing

includesIndexCreation :: [Statement] -> Bool
includesIndexCreation = any isIndexCreation
  where isIndexCreation CreateIndex {} = True
        isIndexCreation _              = False
