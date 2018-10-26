module Validator where

import           Lib
import           Numeric.Natural

validate :: Migration -> Either String Migration
validate migration = do
  indexCreationIsolation migration

indexCreationIsolation :: Migration -> Either String Migration
indexCreationIsolation migration =
  let Migration statements _ _ = migration
      isCreateIndex (CreateIndex _ _ _ _) = True
      isCreateIndex _                     = False
  in if any isCreateIndex statements && (length statements) > 1
     then Left "Perform index creation in its own migration"
     else Right migration

calculateLockTimeout :: [Statement] -> Natural
calculateLockTimeout _ = 0

calculateStatementTimeout :: [Statement] -> Natural
calculateStatementTimeout _ = 0
