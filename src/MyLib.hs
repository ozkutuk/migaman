{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module MyLib where

import Control.Monad (replicateM)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.Beam
  ( Beamable
  , Columnar
  , Database
  , DatabaseSettings
  , Table (PrimaryKey)
  , TableEntity
  )
import Database.Beam qualified as Beam
import Database.Beam.Backend.SQL.BeamExtensions qualified as Beam
import Database.Beam.Sqlite qualified as Beam
import Database.SQLite.Simple qualified as Sqlite
import GHC.Generics (Generic)
import Migadu ()
import Migadu qualified
import Options.Applicative qualified as Opt
import System.Exit (die)
import System.Random.Stateful qualified as RandomS
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

withAuth :: (Migadu.MigaduAuth -> IO a) -> IO a
withAuth f = do
  mAuth <- Migadu.getAuth
  case mAuth of
    Nothing -> die "invalid auth"
    Just auth -> f auth

migaduIdentities :: ImportOptions -> IO (Migadu.Identities Migadu.Read)
migaduIdentities options =
  let idAct = Migadu.IdentitiesIndex options.domain options.target
   in withAuth $ flip Migadu.runMigadu idAct

toIdentityTableImported
  :: ImportOptions -> Migadu.Identity Migadu.Read -> IdentityTable (Beam.QExpr Beam.Sqlite s)
toIdentityTableImported options identity =
  toIdentityTable ("imported-" <> identity.localPart) options.domain options.target identity

toIdentityTable
  :: Text
  -- ^ Account name
  -> Text
  -- ^ Domain
  -> Text
  -- ^ Target
  -> Migadu.Identity Migadu.Read
  -> IdentityTable (Beam.QExpr Beam.Sqlite s)
toIdentityTable account domain target identity =
  Identity'
    { id = Beam.default_
    , account = Beam.val_ account
    , localpart = Beam.val_ identity.localPart
    , domain = Beam.val_ domain
    , target = Beam.val_ target
    }

toIdentityTableAll :: ImportOptions -> Migadu.Identities Migadu.Read -> [IdentityTable (Beam.QExpr Beam.Sqlite s)]
toIdentityTableAll options (Migadu.Identities ids) = map (toIdentityTableImported options) ids

importIdentities' :: ImportOptions -> Migadu.Identities Migadu.Read -> Beam.SqlInsert Beam.Sqlite IdentityTable
importIdentities' options =
  (\d -> Beam.insertOnConflict migamanDb.identity d Beam.anyConflict Beam.onConflictDoNothing)
    . Beam.insertData
    . toIdentityTableAll options

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

randomElement :: RandomS.StatefulGen g m => [a] -> g -> m a
randomElement xs g = (xs !!) <$> RandomS.uniformRM (0, length xs - 1) g

randomText :: RandomS.StatefulGen g m => Int -> g -> m Text
randomText len = fmap T.pack . replicateM len . randomElement validChars
  where
    validChars = ['a' .. 'z'] <> ['0' .. '9']

generateAlias :: GenerateOptions -> Sqlite.Connection -> IO ()
generateAlias options conn = do
  alias <- randomText 10 RandomS.globalStdGen
  let newIdentity = Migadu.defaultCreateIdentity options.userName alias
      createIdentity = Migadu.IdentitiesCreate options.domain options.target newIdentity
  createdIdentity <- withAuth $ flip Migadu.runMigadu createIdentity
  Beam.runBeamSqlite conn
    $ Beam.runInsert
      . Beam.insert migamanDb.identity
      . Beam.insertData
      . (: [])
      . toIdentityTable options.accountName options.domain options.target
    $ createdIdentity
  TIO.putStrLn createdIdentity.address

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
      <> Opt.command
        "generate"
        ( Opt.info
            (GenerateAlias <$> generateOptions)
            (Opt.progDesc "Generate a new alias")
        )
  where
    importOptions :: Opt.Parser ImportOptions
    importOptions =
      ImportOptions
        <$> Opt.strOption
          (Opt.long "domain" <> Opt.help "Domain of the target mailbox")
        <*> Opt.strOption
          (Opt.long "target" <> Opt.help "Local part of the target mailbox")

    generateOptions :: Opt.Parser GenerateOptions
    generateOptions =
      GenerateOptions
        <$> Opt.strOption
          (Opt.long "domain" <> Opt.metavar "DOMAIN" <> Opt.help "Domain of the target mailbox")
        <*> Opt.strOption
          (Opt.long "target" <> Opt.metavar "TARGET" <> Opt.help "Local part of the target mailbox")
        <*> Opt.strOption
          (Opt.long "name" <> Opt.metavar "NAME" <> Opt.help "User name of the generated identity")
        <*> Opt.strArgument
          (Opt.metavar "ACCOUNT")

data Command
  = ListAliases
  | ImportIdentities ImportOptions
  | GenerateAlias GenerateOptions

data ImportOptions = ImportOptions
  { domain :: Text
  , target :: Text
  }

data GenerateOptions = GenerateOptions
  { domain :: Text
  , target :: Text
  , userName :: Text
  , accountName :: Text
  }

main :: IO ()
main = do
  command <- Opt.execParser opts
  conn <- Sqlite.open "database.sqlite3"
  case command of
    ListAliases -> listAliases conn
    ImportIdentities options -> importIdentities options conn
    GenerateAlias options -> generateAlias options conn
  where
    opts :: Opt.ParserInfo Command
    opts = Opt.info (parser Opt.<**> Opt.helper) Opt.fullDesc
