{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MyLib where

import Cli (Command (..), GenerateOptions, GlobalOptions, ImportOptions)
import Cli qualified
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as Embed
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
import System.Directory qualified as Dir
import System.Exit (die)
import System.Random.Stateful qualified as RandomS
import TOML qualified as Toml
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

migaduIdentities :: ImportOptions -> Migadu.MigaduAuth -> IO (Migadu.Identities Migadu.Read)
migaduIdentities options auth =
  let idAct = Migadu.IdentitiesIndex options.domain options.target
   in Migadu.runMigadu auth idAct

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

importIdentities :: ImportOptions -> Migadu.MigaduAuth -> Sqlite.Connection -> IO ()
importIdentities options auth conn = do
  identities <- migaduIdentities options auth
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

generateAlias :: GenerateOptions -> Migadu.MigaduAuth -> Sqlite.Connection -> IO ()
generateAlias options auth conn = do
  alias <- randomText 10 RandomS.globalStdGen
  let newIdentity = Migadu.defaultCreateIdentity options.userName alias
      createIdentity = Migadu.IdentitiesCreate options.domain options.target newIdentity
  createdIdentity <- Migadu.runMigadu auth createIdentity
  Beam.runBeamSqlite conn
    $ Beam.runInsert
      . Beam.insert migamanDb.identity
      . Beam.insertData
      . (: [])
      . toIdentityTable options.accountName options.domain options.target
    $ createdIdentity
  TIO.putStrLn createdIdentity.address

ensureConfigFile :: IO FilePath
ensureConfigFile = do
  configPath <- Dir.getXdgDirectory Dir.XdgConfig "migaman.toml"
  configExists <- Dir.doesFileExist configPath
  if configExists
    then pure configPath
    else do
      BS.writeFile configPath configContents
      die $
        unlines
          [ "Configuration file written to: " <> configPath
          , "Please fill the required fields in the configuration file and run again."
          ]
  where
    configContents :: ByteString
    configContents = $(Embed.embedFileRelative "migaman.toml.sample")

main :: IO ()
main = do
  (globals, cmd) <- Opt.execParser opts
  configPath <- ensureConfigFile
  config <- decodeFileOrDie Cli.configDecoder configPath
  let env = Cli.merge globals cmd config
  conn <- Sqlite.open env.dbPath
  case env.command of
    ListAliases -> listAliases conn
    ImportIdentities options -> importIdentities options env.auth conn
    GenerateAlias options -> generateAlias options env.auth conn
  where
    opts :: Opt.ParserInfo (GlobalOptions, Command)
    opts = Opt.info (Cli.parser Opt.<**> Opt.helper) Opt.fullDesc

    decodeFileOrDie :: Toml.Decoder a -> FilePath -> IO a
    decodeFileOrDie d path = do
      file <- TIO.readFile path
      case Toml.decodeWith d file of
        Left e -> die $ case e of
          Toml.ParseError _ ->
            unlines
              [ T.unpack (Toml.renderTOMLError e)
              , ""
              , "Make sure that you have filled in the configuration file properly and try again."
              ]
          _ -> T.unpack (Toml.renderTOMLError e)
        Right x -> pure x
