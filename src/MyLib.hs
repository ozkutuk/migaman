{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module MyLib where

import Database.Beam
  ( Beamable
  , Columnar
  , Database
  , DatabaseSettings
  , Table (PrimaryKey)
  , TableEntity
  )
import Database.Beam qualified as Beam
import Database.Beam.Sqlite qualified as Beam
import Database.SQLite.Simple qualified as Sqlite

import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

import Migadu ()
import Migadu qualified
import System.Exit (die)

data IdentityTable f = Identity'
  { id :: Columnar f Int64
  , account :: Columnar f Text
  , localpart :: Columnar f Text
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type Identity' = IdentityTable Identity
type IdentityId = PrimaryKey IdentityTable Identity

deriving instance Show Identity'
deriving instance Eq Identity'

instance Table IdentityTable where
  data PrimaryKey IdentityTable column = IdentityId (Columnar column Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)

  primaryKey :: IdentityTable column -> PrimaryKey IdentityTable column
  primaryKey identity = IdentityId identity.id

newtype MigamanDb f = MigamanDb
  {identity :: f (TableEntity IdentityTable)}
  deriving stock (Generic)
  deriving anyclass (Database be)

migamanDb :: DatabaseSettings be MigamanDb
migamanDb = Beam.defaultDbSettings

migaduIdentities :: IO (Migadu.Identities Migadu.Read)
migaduIdentities = do
  mAuth <- Migadu.getAuth
  case mAuth of
    Nothing -> die "invalid auth"
    Just auth ->
      let idAct = Migadu.IdentitiesIndex "example.com" "target"
       in Migadu.runMigadu auth idAct

toIdentityTable :: Migadu.Identity Migadu.Read -> IdentityTable (Beam.QExpr Beam.Sqlite s)
toIdentityTable identity =
  Identity'
    { id = Beam.default_
    , account = Beam.val_ identity.localPart
    , localpart = Beam.val_ identity.localPart
    }

toIdentityTableAll :: Migadu.Identities Migadu.Read -> [IdentityTable (Beam.QExpr Beam.Sqlite s)]
toIdentityTableAll (Migadu.Identities ids) = map toIdentityTable ids

importIdentities' :: Migadu.Identities Migadu.Read -> Beam.SqlInsert Beam.Sqlite IdentityTable
importIdentities' = Beam.insert migamanDb.identity . Beam.insertData . toIdentityTableAll

importIdentities :: IO ()
importIdentities = do
  identities <- migaduIdentities
  conn <- Sqlite.open "database.sqlite3"
  Beam.runBeamSqliteDebug putStrLn conn $ do
    Beam.runInsert $ importIdentities' identities

main :: IO ()
main = do
  importIdentities
