{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Reverse (reverse) where

import           Core
import           Prelude  hiding (reverse)
import           Validate

reverse :: Migration -> Maybe Migration
reverse Migration { statements } =
  case traverse reverseStatement statements of
    Nothing -> Nothing
    Just ss -> Just Migration { statements = ss
                              , lockTimeout = calculateLockTimeout ss
                              , statementTimeout = calculateStatementTimeout ss
                              , transaction = determineTransaction ss }


reverseStatement :: Statement -> Maybe Statement
reverseStatement AddColumn { table, column } = Just DropColumn { table, column }
reverseStatement DropColumn {}               = Nothing
reverseStatement CreateTable { table }        = Just DropTable { table }
reverseStatement DropTable {} = Nothing
reverseStatement MakeColumnNotNull { table, column } = Just MakeColumnNull { table, column }
reverseStatement MakeColumnNull { table, column } = Just MakeColumnNotNull { table, column }
reverseStatement ChangeColumnDefault { table, column, from, to } = Just ChangeColumnDefault { table, column, from = to, to = from }
reverseStatement DropColumnDefault { table, column, from } = Just ChangeColumnDefault { table, column, from = "", to = from }
reverseStatement CreateIndex { name } = Just DropIndex { name }
reverseStatement DropIndex {} = Nothing
reverseStatement AddForeignKey { name } = Just DropConstraint { name }
reverseStatement DropConstraint {} = Nothing
