{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module MyLib where

import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
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
import GHC.Generics (Generic)
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
  , domain :: Columnar f Text
  , target :: Columnar f Text
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

migaduIdentities :: ImportOptions -> IO (Migadu.Identities Migadu.Read)
migaduIdentities options = do
  mAuth <- Migadu.getAuth
  case mAuth of
    Nothing -> die "invalid auth"
    Just auth ->
      let idAct = Migadu.IdentitiesIndex options.domain options.target
       in Migadu.runMigadu auth idAct

toIdentityTable :: ImportOptions -> Migadu.Identity Migadu.Read -> IdentityTable (Beam.QExpr Beam.Sqlite s)
toIdentityTable options identity =
  Identity'
    { id = Beam.default_
    , account = Beam.val_ ("imported-" <> identity.localPart)
    , localpart = Beam.val_ identity.localPart
    , domain = Beam.val_ options.domain
    , target = Beam.val_ options.target
    }

toIdentityTableAll :: ImportOptions -> Migadu.Identities Migadu.Read -> [IdentityTable (Beam.QExpr Beam.Sqlite s)]
toIdentityTableAll options (Migadu.Identities ids) = map (toIdentityTable options) ids

importIdentities' :: ImportOptions -> Migadu.Identities Migadu.Read -> Beam.SqlInsert Beam.Sqlite IdentityTable
importIdentities' options = Beam.insert migamanDb.identity . Beam.insertData . toIdentityTableAll options

importIdentities :: ImportOptions -> Sqlite.Connection -> IO ()
importIdentities options conn = do
  identities <- migaduIdentities options
  Beam.runBeamSqlite conn $ do
    Beam.runInsert $ importIdentities' options identities

-- I will almost certainly replace this with a prettyprinter solution,
-- but this is alright for the time being
tabulateAliases :: [Identity'] -> Tabular.Table Text Text Text
tabulateAliases identities =
  Tabular.Table
    (Tabular.Group Tabular.SingleLine (headerAccountNames identities))
    (Tabular.Group Tabular.SingleLine $ map Tabular.Header ["email", "target"])
    (map mkRow identities)
  where
    mkRow :: Identity' -> [Text]
    mkRow identity =
      let mkEmail = (<> "@" <> identity.domain)
       in [mkEmail identity.localpart, mkEmail identity.target]

    headerAccountNames :: [Identity'] -> [Tabular.Header Text]
    headerAccountNames = map (Tabular.Header . (.account))

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
            (ImportIdentities <$> importOptions)
            (Opt.progDesc "Import identities from Migadu as aliases")
        )
  where
    importOptions :: Opt.Parser ImportOptions
    importOptions =
      ImportOptions
        <$> Opt.strOption
          (Opt.long "domain" <> Opt.help "Domain of the target mailbox")
        <*> Opt.strOption
          (Opt.long "target" <> Opt.help "Local part of the target mailbox")

data Command
  = ListAliases
  | ImportIdentities ImportOptions

data ImportOptions = ImportOptions
  { domain :: Text
  , target :: Text
  }

main :: IO ()
main = do
  command <- Opt.execParser opts
  conn <- Sqlite.open "database.sqlite3"
  case command of
    ListAliases -> listAliases conn
    ImportIdentities options -> importIdentities options conn
  where
    opts :: Opt.ParserInfo Command
    opts = Opt.info (parser Opt.<**> Opt.helper) Opt.fullDesc
