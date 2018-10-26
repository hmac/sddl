{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Reverse (reverse) where

import           Lib
import           Prelude   hiding (reverse)
import           Validator

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
reverseStatement CreateTable { name }        = Just DropTable { name }
reverseStatement DropTable {} = Nothing
reverseStatement MakeColumnNotNull { table, column } = Just MakeColumnNull { table, column }
reverseStatement MakeColumnNull { table, column } = Just MakeColumnNotNull { table, column }
reverseStatement ChangeColumnDefault { table, column, from, to } = Just ChangeColumnDefault { table, column, from = to, to = from }
reverseStatement CreateIndex { name } = Just DropIndex { name }
reverseStatement DropIndex {} = Nothing
reverseStatement AddForeignKey { name } = Just DropConstraint { name }
reverseStatement DropConstraint {} = Nothing
