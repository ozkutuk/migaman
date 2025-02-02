{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Cli where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Migadu qualified
import Options.Applicative qualified as Opt
import System.Exit (die)
import TOML qualified as Toml

command :: Opt.Parser (Command OptionPhase)
command =
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
        <$> Opt.optional
          ( Opt.strOption
              (Opt.long "domain" <> Opt.help "Domain of the target mailbox")
          )
        <*> Opt.optional
          ( Opt.strOption
              (Opt.long "target" <> Opt.help "Local part of the target mailbox")
          )

    generateOptions :: Opt.Parser GenerateOptions
    generateOptions =
      GenerateOptions
        <$> Opt.optional
          ( Opt.strOption
              (Opt.long "domain" <> Opt.metavar "DOMAIN" <> Opt.help "Domain of the target mailbox")
          )
        <*> Opt.optional
          ( Opt.strOption
              (Opt.long "target" <> Opt.metavar "TARGET" <> Opt.help "Local part of the target mailbox")
          )
        <*> Opt.optional
          ( Opt.strOption
              (Opt.long "name" <> Opt.metavar "NAME" <> Opt.help "User name of the generated identity")
          )
        <*> Opt.strArgument
          (Opt.metavar "ACCOUNT")

globalOptions :: Opt.Parser GlobalOptions
globalOptions =
  fmap GlobalOptions . Opt.optional $
    Opt.strOption
      ( Opt.long "database"
          <> Opt.metavar "FILE"
          <> Opt.help "Path of the SQLite database"
      )

type data Phase = OptionPhase | EnvPhase

data Command (phase :: Phase)
  = ListAliases
  | ImportIdentities (ImportCommand phase)
  | GenerateAlias (GenerateCommand phase)

type family ImportCommand (phase :: Phase) where
  ImportCommand OptionPhase = ImportOptions
  ImportCommand EnvPhase = ImportEnv

type family GenerateCommand (phase :: Phase) where
  GenerateCommand OptionPhase = GenerateOptions
  GenerateCommand EnvPhase = GenerateEnv

data GlobalOptions = GlobalOptions
  { dbPath :: Maybe FilePath
  }

data ImportOptions = ImportOptions
  { domain :: Maybe Text
  , target :: Maybe Text
  }

data ImportEnv = ImportEnv
  { domain :: Text
  , target :: Text
  }

data GenerateOptions = GenerateOptions
  { domain :: Maybe Text
  , target :: Maybe Text
  , userName :: Maybe Text
  , accountName :: Text
  }

data GenerateEnv = GenerateEnv
  { domain :: Text
  , target :: Text
  , userName :: Text
  , accountName :: Text
  }

data Config = Config
  { auth :: Migadu.MigaduAuth
  , dbPath :: FilePath
  , defaults :: Defaults
  }

data Defaults = Defaults
  { domain :: Maybe Text
  , target :: Maybe Text
  , userName :: Maybe Text
  }

data Env = Env
  { dbPath :: FilePath
  , auth :: Migadu.MigaduAuth
  , command :: Command EnvPhase
  }

configDecoder :: Toml.Decoder Config
configDecoder =
  Config
    <$> Toml.getFieldsWith authDecoder ["migadu", "auth"]
    <*> Toml.getFields ["migaman", "database"]
    <*> Toml.getFieldsWith defaultsDecoder ["migaman", "defaults"]
  where
    authDecoder :: Toml.Decoder Migadu.MigaduAuth
    authDecoder =
      let account = T.encodeUtf8 <$> Toml.getField "account"
          key = T.encodeUtf8 <$> Toml.getField "key"
       in Migadu.mkAuth <$> account <*> key

    defaultsDecoder :: Toml.Decoder Defaults
    defaultsDecoder =
      let domain = Toml.getFieldOpt "domain"
          target = Toml.getFieldOpt "target"
          userName = Toml.getFieldOpt "name"
       in Defaults <$> domain <*> target <*> userName

parser :: Opt.Parser (GlobalOptions, Command OptionPhase)
parser = (,) <$> globalOptions <*> command

merge :: GlobalOptions -> Command OptionPhase -> Config -> IO Env
merge globals cmd config = Env dbPath auth <$> cmd'
  where
    auth :: Migadu.MigaduAuth
    auth = config.auth

    dbPath :: FilePath
    dbPath = fromMaybe config.dbPath globals.dbPath

    cmd' :: IO (Command EnvPhase)
    cmd' = case cmd of
      ListAliases -> pure ListAliases
      GenerateAlias opts ->
        GenerateAlias <$> do
          -- optional
          domain <- maybe (optError "domain") pure (opts.domain <|> config.defaults.domain)
          target <- maybe (optError "target") pure (opts.target <|> config.defaults.target)
          userName <- maybe (optError "name") pure (opts.userName <|> config.defaults.userName)
          -- required
          let accountName = opts.accountName
          pure $ GenerateEnv {domain, target, userName, accountName}
      ImportIdentities opts ->
        ImportIdentities <$> do
          -- optional
          domain <- maybe (optError "domain") pure (opts.domain <|> config.defaults.domain)
          target <- maybe (optError "target") pure (opts.target <|> config.defaults.target)
          pure $ ImportEnv {domain, target}
      where
        optError :: String -> IO a
        optError opt = die $ "option not set: " <> opt
