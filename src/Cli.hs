{-# LANGUAGE OverloadedStrings #-}

module Cli where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Migadu qualified
import Options.Applicative qualified as Opt
import TOML qualified as Toml

command :: Opt.Parser Command
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

globalOptions :: Opt.Parser GlobalOptions
globalOptions =
  fmap GlobalOptions . Opt.optional $
    Opt.strOption
      ( Opt.long "database"
          <> Opt.metavar "FILE"
          <> Opt.help "Path of the SQLite database"
      )

data Command
  = ListAliases
  | ImportIdentities ImportOptions
  | GenerateAlias GenerateOptions

data GlobalOptions = GlobalOptions
  { dbPath :: Maybe FilePath
  }

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

data Config = Config
  { auth :: Migadu.MigaduAuth
  , dbPath :: FilePath
  }

data Env = Env
  { dbPath :: FilePath
  , auth :: Migadu.MigaduAuth
  , command :: Command
  }

configDecoder :: Toml.Decoder Config
configDecoder =
  Config
    <$> Toml.getFieldsWith authDecoder ["migadu", "auth"]
    <*> Toml.getFields ["migaman", "database"]
  where
    authDecoder :: Toml.Decoder Migadu.MigaduAuth
    authDecoder =
      let account = T.encodeUtf8 <$> Toml.getField "account"
          key = T.encodeUtf8 <$> Toml.getField "key"
       in Migadu.mkAuth <$> account <*> key

parser :: Opt.Parser (GlobalOptions, Command)
parser = (,) <$> globalOptions <*> command

merge :: GlobalOptions -> Command -> Config -> Env
merge globals cmd config = Env dbPath auth cmd'
  where
    auth :: Migadu.MigaduAuth
    auth = config.auth

    dbPath :: FilePath
    dbPath = fromMaybe config.dbPath globals.dbPath

    cmd' :: Command
    cmd' = cmd
