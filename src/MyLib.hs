{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib where

import Cli (Command (..), GlobalOptions)
import Cli qualified
import Control.Monad (replicateM, (<=<))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as Embed
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple qualified as Sqlite
import IdentityTable.Model (Identity', IdentityTable (..))
import IdentityTable.Query qualified as Query
import Migadu qualified
import Options.Applicative qualified as Opt
import System.Directory qualified as Dir
import System.Exit (die)
import System.Random.Stateful qualified as RandomS
import TOML qualified as Toml
import Text.Tabular qualified as Tabular
import Text.Tabular.AsciiArt qualified as Tabular

importIdentities :: Cli.ImportEnv -> Migadu.MigaduAuth -> Sqlite.Connection -> IO ()
importIdentities options auth conn = do
  identities <- migaduIdentities
  let namedIds = generateImportNames identities
  Query.insertIdentities options.domain options.target namedIds conn
  where
    migaduIdentities :: IO (Migadu.Identities Migadu.Read)
    migaduIdentities =
      let idAct = Migadu.IdentitiesIndex options.domain options.target
       in Migadu.runMigadu auth idAct

    generateImportNames :: Migadu.Identities Migadu.Read -> [(Text, Migadu.Identity Migadu.Read)]
    generateImportNames (Migadu.Identities ids) = map attachName ids
      where
        attachName :: Migadu.Identity Migadu.Read -> (Text, Migadu.Identity Migadu.Read)
        attachName id' = ("imported-" <> id'.localPart, id')

listAliases :: Sqlite.Connection -> IO ()
listAliases = (putStrLn . formatAliases) <=< Query.getIdentities
  where
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

generateAlias :: Cli.GenerateEnv -> Migadu.MigaduAuth -> Sqlite.Connection -> IO ()
generateAlias options auth conn = do
  alias <- randomText 10 RandomS.globalStdGen
  let newIdentity = Migadu.defaultCreateIdentity options.userName alias
      createIdentity = Migadu.IdentitiesCreate options.domain options.target newIdentity
  createdIdentity <- Migadu.runMigadu auth createIdentity
  Query.insertIdentity options.accountName options.domain options.target createdIdentity conn
  TIO.putStrLn createdIdentity.address
  where
    randomElement :: RandomS.StatefulGen g m => [a] -> g -> m a
    randomElement xs g = (xs !!) <$> RandomS.uniformRM (0, length xs - 1) g

    randomText :: RandomS.StatefulGen g m => Int -> g -> m Text
    randomText len = fmap T.pack . replicateM len . randomElement validChars
      where
        validChars = ['a' .. 'z'] <> ['0' .. '9']

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
  env <- Cli.merge globals cmd config
  conn <- Sqlite.open env.dbPath
  case env.command of
    ListAliases -> listAliases conn
    ImportIdentities options -> importIdentities options env.auth conn
    GenerateAlias options -> generateAlias options env.auth conn
  where
    opts :: Opt.ParserInfo (GlobalOptions, Command Cli.OptionPhase)
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
