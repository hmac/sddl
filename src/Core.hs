{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Core where

import           Data.Aeson      (FromJSON, Options (..), camelTo2,
                                  defaultOptions, genericParseJSON, parseJSON,
                                  withText)
import           GHC.Generics

import           Numeric.Natural

data SqlType = SText | STimestamp | SInteger | SBoolean | SJson
  deriving (Eq, Show, Generic)

instance FromJSON SqlType where
  parseJSON = withText "a SQL type" $ \t ->
    case t of
      "text"      -> pure SText
      "timestamp" -> pure STimestamp
      "integer"   -> pure SInteger
      "boolean"   -> pure SBoolean
      "json"      -> pure SJson
      s           -> fail $ "Invalid SQL type " ++ show s

data Statement =
    AddColumn { table :: String, column :: String, type_ :: SqlType }
  | DropColumn { table :: String, column :: String }
  | CreateTable { name :: String, prefix :: String, definition :: [ColDef] }
  | DropTable { name :: String }
  | MakeColumnNotNull { table :: String, column :: String }
  | MakeColumnNull { table :: String, column :: String }
  | ChangeColumnDefault { table :: String, column :: String, from :: String, to :: String }
  | CreateIndex { table :: String, columns :: [String], name :: String, partial :: Maybe String }
  | DropIndex { name :: String }
  | AddForeignKey { sourceTable  :: String
                  , sourceColumn :: String
                  , targetTable  :: String
                  , targetColumn :: String
                  , name         :: String
                  }
  | DropConstraint { name :: String }
  deriving (Eq, Show, Generic)

instance FromJSON Statement where
  parseJSON = genericParseJSON defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , fieldLabelModifier = camelTo2 '_' . filter (/= '_') }

data ColDef =
  ColDef { name :: String, type_ :: SqlType, null :: Bool }
  deriving (Eq, Show, Generic)

instance FromJSON ColDef where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . filter (/= '_') }

data Migration = Migration { statements       :: [Statement]
                           , lockTimeout      :: Natural
                           , statementTimeout :: Natural
                           , transaction      :: Bool }
  deriving (Eq, Show, Generic)
instance FromJSON Migration

newtype LockTimeout = LockTimeout Int deriving (Eq, Show, Generic)
instance FromJSON LockTimeout

newtype StatementTimeout = StatementTimeout Int deriving (Eq, Show, Generic)
instance FromJSON StatementTimeout