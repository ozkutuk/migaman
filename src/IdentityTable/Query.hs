module IdentityTable.Query (insertIdentity, insertIdentities, getIdentities, getIdentity, toggleIdentity) where

import Data.Text (Text)
import Database.Beam qualified as Beam
import Database.Beam.Backend.SQL.BeamExtensions qualified as Beam
import Database.Beam.Sqlite qualified as Beam
import Database.SQLite.Simple qualified as Sqlite
import IdentityTable.Model qualified as Model
import Migadu qualified

getIdentities :: Sqlite.Connection -> IO [Model.Identity']
getIdentities conn = do
  let allAliases = Beam.all_ Model.migamanDb.identity
  Beam.runBeamSqlite conn $ do
    Beam.runSelectReturningList $ Beam.select allAliases

getIdentity :: Text -> Sqlite.Connection -> IO (Maybe Model.Identity')
getIdentity accountName conn = do
  let account =
        Beam.filter_ (\acc -> acc.account Beam.==. Beam.val_ accountName) $
          Beam.all_ Model.migamanDb.identity
  Beam.runBeamSqliteDebug putStrLn conn $ do
    Beam.runSelectReturningOne $ Beam.select account

toggleIdentity :: Bool -> Text -> Sqlite.Connection -> IO ()
toggleIdentity enabled accountName conn = do
  Beam.runBeamSqliteDebug putStrLn conn $ do
    Beam.runUpdate $
      Beam.update
        Model.migamanDb.identity
        (\acc -> acc.enabled Beam.<-. Beam.val_ enabled)
        (\acc -> acc.account Beam.==. Beam.val_ accountName)

insertIdentity
  :: Text
  -- ^ Account name
  -> Text
  -- ^ Domain
  -> Text
  -- ^ Target
  -> Migadu.Identity Migadu.Read
  -> Sqlite.Connection
  -> IO ()
insertIdentity account domain target identity conn =
  Beam.runBeamSqlite conn
    . Beam.runInsert
    . Beam.insert Model.migamanDb.identity
    . Beam.insertData
    $ [identityToInsert]
  where
    identityToInsert :: Model.IdentityTable (Beam.QExpr Beam.Sqlite s)
    identityToInsert = toIdentityTable account domain target identity

toIdentityTable
  :: Text
  -- ^ Account name
  -> Text
  -- ^ Domain
  -> Text
  -- ^ Target
  -> Migadu.Identity Migadu.Read
  -> Model.IdentityTable (Beam.QExpr Beam.Sqlite s)
toIdentityTable account domain target identity =
  Model.Identity'
    { id = Beam.default_
    , account = Beam.val_ account
    , localpart = Beam.val_ identity.localPart
    , domain = Beam.val_ domain
    , target = Beam.val_ target
    , enabled = Beam.default_
    }

insertIdentities
  :: Text
  -- ^ Domain
  -> Text
  -- ^ Target
  -> [(Text, Migadu.Identity Migadu.Read)]
  -- ^ Identities paired with the account name
  -> Sqlite.Connection
  -> IO ()
insertIdentities domain target identities conn =
  Beam.runBeamSqlite conn $
    Beam.runInsert insertIdentities'
  where
    insertIdentities' :: Beam.SqlInsert Beam.Sqlite Model.IdentityTable
    insertIdentities' =
      Beam.insertOnConflict
        Model.migamanDb.identity
        (Beam.insertData toIdentityTableAll)
        Beam.anyConflict
        Beam.onConflictDoNothing

    toIdentityTableAll :: [Model.IdentityTable (Beam.QExpr Beam.Sqlite s)]
    toIdentityTableAll = map toTable identities

    toTable :: (Text, Migadu.Identity Migadu.Read) -> Model.IdentityTable (Beam.QExpr Beam.Sqlite s)
    toTable (accountName, identity) = toIdentityTable accountName domain target identity

-- where
--   toIdentityTableImported
--     :: Migadu.Identity Migadu.Read -> Model.IdentityTable (Beam.QExpr Beam.Sqlite s)
--   toIdentityTableImported identity =
--     toIdentityTable ("imported-" <> identity.localPart) domain target identity
