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

import Data.Text qualified as T
import Migadu ()
import Migadu qualified
import Options.Applicative qualified as Opt
import System.Exit (die)
import Text.Tabular qualified as Tabular
import Text.Tabular.AsciiArt qualified as Tabular

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

importIdentities :: Sqlite.Connection -> IO ()
importIdentities conn = do
  identities <- migaduIdentities
  Beam.runBeamSqlite conn $ do
    Beam.runInsert $ importIdentities' identities

-- I will almost certainly replace this with a prettyprinter solution,
-- but this is alright for the time being
tabulateAliases :: [Identity'] -> Tabular.Table Text Text Text
tabulateAliases identities =
  Tabular.Table
    (Tabular.Group Tabular.SingleLine (g identities))
    (Tabular.Group Tabular.SingleLine [Tabular.Header "email"])
    (map f identities)
  where
    f :: Identity' -> [Text]
    f identity = [identity.account, identity.localpart]

    g :: [Identity'] -> [Tabular.Header Text]
    g = map (Tabular.Header . (.account))

formatAliases :: [Identity'] -> String
formatAliases = Tabular.render T.unpack T.unpack T.unpack . tabulateAliases

listAliases :: Sqlite.Connection -> IO ()
listAliases conn = do
  let allAliases = Beam.all_ migamanDb.identity
  aliases <- Beam.runBeamSqlite conn $ do
    Beam.runSelectReturningList $ Beam.select allAliases
  putStrLn $ formatAliases aliases

parser :: Opt.Parser Command
parser =
  Opt.hsubparser $
    Opt.command
      "list"
      ( Opt.info
          (pure ListAliases)
          (Opt.progDesc "List aliases")
      )
      <> Opt.command
        "import"
        ( Opt.info
            (pure ImportIdentities)
            (Opt.progDesc "Import identities from Migadu as aliases")
        )

data Command
  = ListAliases
  | ImportIdentities

main :: IO ()
main = do
  command <- Opt.execParser opts
  conn <- Sqlite.open "database.sqlite3"
  case command of
    ListAliases -> listAliases conn
    ImportIdentities -> importIdentities conn
  where
    opts :: Opt.ParserInfo Command
    opts = Opt.info (parser Opt.<**> Opt.helper) Opt.fullDesc
