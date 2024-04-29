{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ImportStylePlugin.Config where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), FromJSONKey, Value (..), withObject, (.:))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.String (IsString)
import GHC.Generics (Generic)

-- | Report either error or warning during compilation
data Severity = Error | Warning
  deriving (Generic, FromJSON, Show)

data Qualification = Required | Forbidden
  deriving (Generic, FromJSON, Show)

data ModuleAliases = Exactly (Set String) | OrOmitted (Set String)
  deriving (Show)

data NamesList = BlackList (Set String) | WhiteList (Set String)
  deriving (Show)

data ImportRule = ImportRule
  { qualification :: Maybe Qualification
  -- ^ @'Nothing'@ means "don\'t care"
  , aliases :: Maybe ModuleAliases
  -- ^ @'Nothing'@ means "don\'t care"
  , importedNames :: Maybe NamesList
  -- ^ @'Nothing'@ means "don\'t care"
  }
  deriving (Generic, FromJSON, Show)

data QualificationStyle = Post | Pre
  deriving (Generic, FromJSON, Show)

data ImportRules = ImportRules
  { rules :: [ImportRule]
  , severity :: Severity
  }
  deriving (Generic, FromJSON, Show)

data Ban = Ban
  { severity :: Severity
  , why :: String
  -- ^ ban reason
  }
  deriving (Generic, FromJSON, Show)

data ImportsStyle
  = ImportsStyle
  { qualificationStyle :: Maybe QualificationStyle
  -- ^ @'Nothing'@ means "don\'t care"
  , bannedModules :: Map.Map ModuleName Ban
  , importRules :: Map.Map ModuleName ImportRules
  }
  deriving (Generic, FromJSON, Show)

newtype ModuleName = ModuleName String
  deriving (Eq, Ord)
  deriving newtype (FromJSONKey, IsString, Show)

instance FromJSON NamesList where
  parseJSON = withObject "NamesList" \o ->
    BlackList <$> o .: "blacklist" <|> WhiteList <$> o .: "whitelist"

instance FromJSON ModuleAliases where
  parseJSON (Object o) = Exactly <$> o .: "exactly"
  parseJSON o = OrOmitted <$> parseJSON o
