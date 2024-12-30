{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

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

main :: IO ()
main = do
  conn <- Sqlite.open "database.sqlite3"
  Beam.runBeamSqliteDebug putStrLn conn $ do
    identities <- Beam.runSelectReturningList $ Beam.select (Beam.all_ migamanDb.identity)
    liftIO $ print identities
