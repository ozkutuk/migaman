{-# LANGUAGE OverloadedStrings #-}

module Migrations where

import Data.Foldable (traverse_)
import Data.String (fromString)
import Data.Text qualified as T
import Database.Migrant qualified as Migrant
import Database.Migrant.Driver.Sqlite ()
import Database.Migrant.MigrationName (unpackMigrationName)
import Database.SQLite.Simple qualified as Sqlite

runMigrations :: Sqlite.Connection -> IO ()
runMigrations = migrate myMigrations migrateUp migrateDown

migrate
  :: [Migration]
  -> (Migration -> Sqlite.Connection -> IO ())
  -> (Migration -> Sqlite.Connection -> IO ())
  -> Sqlite.Connection
  -> IO ()
migrate migrations up down =
  Migrant.migrate (map toMigrationName migrations) (up . fromMigrationName) (down . fromMigrationName)
  where
    toMigrationName :: Migration -> Migrant.MigrationName
    toMigrationName = fromString . show

    -- I hate this 'read', but Migrant forces the use of MigrationName
    fromMigrationName :: Migrant.MigrationName -> Migration
    fromMigrationName = read . T.unpack . unpackMigrationName

-- NOTE(ozkutuk): order matters, because 'myMigrations' is populated
-- by the Enum instance
data Migration
  = CreateIdentityTable
  | AddDomainAndTargetToIdentity
  | AddEnabledToIdentity
  deriving stock (Show, Read, Ord, Eq, Enum, Bounded)

myMigrations :: [Migration]
myMigrations = [minBound .. maxBound]

migrateUp :: Migration -> Sqlite.Connection -> IO ()
migrateUp name conn = case name of
  CreateIdentityTable ->
    Sqlite.execute_
      conn
      $ fromString
      $ unlines
        [ "CREATE TABLE \"identity\" ("
        , "  \"id\" INTEGER,"
        , "  \"account\" TEXT UNIQUE,"
        , "  \"localpart\" TEXT UNIQUE,"
        , "  PRIMARY KEY(\"id\" AUTOINCREMENT)"
        , ");"
        ]
  AddDomainAndTargetToIdentity ->
    traverse_
      (Sqlite.execute_ conn)
      [ "ALTER TABLE \"identity\" ADD COLUMN \"domain\" TEXT;"
      , "ALTER TABLE \"identity\" ADD COLUMN \"target\" TEXT;"
      ]
  AddEnabledToIdentity ->
    Sqlite.execute_ conn
      "ALTER TABLE \"identity\" ADD COLUMN \"enabled\" INTEGER NOT NULL DEFAULT 1;"

migrateDown :: Migration -> Sqlite.Connection -> IO ()
migrateDown name conn = case name of
  CreateIdentityTable ->
    Sqlite.execute_ conn "DROP TABLE \"identity\";"
  AddDomainAndTargetToIdentity ->
    traverse_
      (Sqlite.execute_ conn)
      [ "ALTER TABLE \"identity\" DROP COLUMN \"domain\";"
      , "ALTER TABLE \"identity\" DROP COLUMN \"target\";"
      ]
  AddEnabledToIdentity ->
    Sqlite.execute_ conn
      "ALTER TABLE \"identity\" DROP COLUMN \"enabled\";"
