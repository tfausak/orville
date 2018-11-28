{-|
Module    : Database.Orville.Internal.MigrationPlan
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP #-}

module Database.Orville.Internal.MigrationPlan
  ( MigrationPlan
  , MigrationItem(..)
  , DDL
  , migrationDDLForItem
  , migrationPlanItems
  ) where

#if 800 < __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif

import qualified Data.DList as DList

import Database.Orville.Internal.Types (SchemaItem)

type DDL = String

data MigrationItem = MigrationItem
  { migrationItemSchemaItem :: SchemaItem
  , migrationItemDDL :: DDL
  }

data MigrationPlan =
  MigrationPlan MigrationItem
                (DList.DList MigrationItem)

migrationDDLForItem :: SchemaItem -> DDL -> MigrationPlan
migrationDDLForItem schemaItem ddl =
  MigrationPlan (MigrationItem schemaItem ddl) DList.empty

append :: MigrationPlan -> MigrationPlan -> MigrationPlan
append (MigrationPlan itemA restA) (MigrationPlan itemB restB) =
  MigrationPlan itemA $ DList.append restA $ DList.cons itemB restB

migrationPlanItems :: MigrationPlan -> [MigrationItem]
migrationPlanItems (MigrationPlan item rest) =
  DList.toList $ DList.cons item rest

#if 800 < __GLASGOW_HASKELL__
instance Semigroup MigrationPlan where
  (<>) = append
#endif
-- GHC 8.2.2 unfortunately needs both to avoid (some) warnings :(
#if __GLASGOW_HASKELL__ < 804
instance Monoid MigrationPlan
  -- MigrationPlan doesn't support mempty, so don't provide a Monoid instance for
  -- base versions that have migrated to Semigroup.
  where
    mempty =
      error
        "mempty for MigrationPlan used, but MigrationPlan cannot be empty! MigrationPlan only support Monoid prior to base 4.11.0"
    mappend = append
#endif
