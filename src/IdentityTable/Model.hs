{-# LANGUAGE TypeFamilies #-}

module IdentityTable.Model where

import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Beam
  ( Beamable
  , Columnar
  , Database
  , DatabaseSettings
  , Table (PrimaryKey)
  , TableEntity
  )
import Database.Beam qualified as Beam
import GHC.Generics (Generic)

data IdentityTable f = Identity'
  { id :: Columnar f Int64
  , account :: Columnar f Text
  , localpart :: Columnar f Text
  , domain :: Columnar f Text
  , target :: Columnar f Text
  , enabled :: Columnar f Bool
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
