{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Migadu
  ( module Migadu.Fields
  , module Migadu.Identity
  , module Migadu.Mailbox
  , MigaduAuth
  , MigaduAuthInput
  , mkAuthInput
  , mkPlainKey
  , mkKeyCommand
  , mkAuth
  , getAuth
  , MigaduRequest (..)
  , runMigadu
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as Req
import System.Environment qualified as Env
import System.Process.Typed qualified as Process
import Prelude hiding (Read)

import Migadu.Fields (MailboxType (..))
import Migadu.Identity (Identities (..), Identity (..), defaultCreateIdentity, defaultUpdateIdentity)
import Migadu.Mailbox (Mailbox, Mailboxes)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Data.Char (isSpace)

baseEndpoint :: Req.Url 'Req.Https
baseEndpoint = Req.https "api.migadu.com" /: "v1"

data MigaduAuth = MigaduAuth
  { account :: ByteString
  , key :: ByteString
  }

data MigaduAuthInput = MigaduAuthInput
  { account :: Text
  , key :: MigaduKey
  }

data MigaduKey
  = KeyPlain Text
  | KeyCommand String

mkPlainKey :: Text -> MigaduKey
mkPlainKey = KeyPlain

mkKeyCommand :: String -> MigaduKey
mkKeyCommand = KeyCommand

mkAuthInput :: Text -> MigaduKey -> MigaduAuthInput
mkAuthInput = MigaduAuthInput

mkAuth :: MigaduAuthInput -> IO MigaduAuth
mkAuth (MigaduAuthInput (T.encodeUtf8 -> account) inKey) =
  case inKey of
    KeyPlain (T.encodeUtf8 -> key) -> pure MigaduAuth{..}
    KeyCommand keyCmd -> do
      let cmd = Process.shell keyCmd
      key <- BSC.dropWhileEnd isSpace . BS.toStrict <$> Process.readProcessStdout_ cmd
      pure MigaduAuth{..}

getAuth :: IO (Maybe MigaduAuth)
getAuth = do
  mAccount <- fmap BSC.pack <$> Env.lookupEnv "MIGADU_ACCOUNT"
  mKey <- fmap BSC.pack <$> Env.lookupEnv "MIGADU_API_KEY"
  pure $ MigaduAuth <$> mAccount <*> mKey

data MigaduRequest a where
  MailboxesIndex :: Text -> MigaduRequest (Mailboxes 'Read)
  MailboxesShow :: Text -> Text -> MigaduRequest (Mailbox 'Read)
  MailboxesCreate :: Text -> Mailbox 'Create -> MigaduRequest (Mailbox 'Read)
  MailboxesDelete :: Text -> Text -> MigaduRequest (Mailbox 'Read)
  IdentitiesIndex :: Text -> Text -> MigaduRequest (Identities 'Read)
  IdentitiesCreate
    :: Text
    -- ^ Domain
    -> Text
    -- ^ Target local part
    -> Identity 'Create
    -> MigaduRequest (Identity 'Read)
  IdentitiesUpdate
    :: Text
    -- ^ Domain
    -> Text
    -- ^ Target local part
    -> Text
    -- ^ Alias local part
    -> Identity 'Update
    -> MigaduRequest (Identity 'Read)

mkAuthOpts :: MigaduAuth -> Req.Option 'Req.Https
mkAuthOpts (MigaduAuth account key) = Req.basicAuth account key

mailboxesIndex :: MigaduAuth -> Text -> Req.Req (Mailboxes 'Read)
mailboxesIndex auth domain =
  Req.responseBody
    <$> Req.req
      Req.GET
      (baseEndpoint /: "domains" /: domain /: "mailboxes")
      Req.NoReqBody
      Req.jsonResponse
      (mkAuthOpts auth)

mailboxesShow :: MigaduAuth -> Text -> Text -> Req.Req (Mailbox 'Read)
-- mailboxesShow :: MigaduAuth -> Text -> Text -> Req.Req Aeson.Value
mailboxesShow auth domain mailbox =
  Req.responseBody
    <$> Req.req
      Req.GET
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: mailbox)
      Req.NoReqBody
      Req.jsonResponse
      (mkAuthOpts auth)

mailboxesCreate :: MigaduAuth -> Text -> Mailbox 'Create -> Req.Req (Mailbox 'Read)
mailboxesCreate auth domain mailbox =
  Req.responseBody
    <$> Req.req
      Req.POST
      (baseEndpoint /: "domains" /: domain /: "mailboxes")
      (Req.ReqBodyJson mailbox)
      Req.jsonResponse
      (mkAuthOpts auth)

mailboxesDelete :: MigaduAuth -> Text -> Text -> Req.Req (Mailbox 'Read)
mailboxesDelete auth domain localPart =
  Req.responseBody
    <$> Req.req
      Req.DELETE
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: localPart)
      Req.NoReqBody
      Req.jsonResponse
      (mkAuthOpts auth)

identitiesIndex :: MigaduAuth -> Text -> Text -> Req.Req (Identities 'Read)
identitiesIndex auth domain localPart =
  Req.responseBody
    <$> Req.req
      Req.GET
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: localPart /: "identities")
      Req.NoReqBody
      Req.jsonResponse
      (mkAuthOpts auth)

identitiesCreate :: MigaduAuth -> Text -> Text -> Identity 'Create -> Req.Req (Identity 'Read)
identitiesCreate auth domain aliasTo identity =
  Req.responseBody
    <$> Req.req
      Req.POST
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: aliasTo /: "identities")
      (Req.ReqBodyJson identity)
      Req.jsonResponse
      (mkAuthOpts auth)

identitiesUpdate :: MigaduAuth -> Text -> Text -> Text -> Identity 'Update -> Req.Req (Identity 'Read)
identitiesUpdate auth domain aliasTo localPart identity =
  Req.responseBody
    <$> Req.req
      Req.PUT
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: aliasTo /: "identities" /: localPart)
      (Req.ReqBodyJson identity)
      Req.jsonResponse
      (mkAuthOpts auth)

runMigadu :: MigaduAuth -> MigaduRequest a -> IO a
runMigadu auth =
  Req.runReq Req.defaultHttpConfig . \case
    MailboxesIndex domain -> mailboxesIndex auth domain
    MailboxesShow domain mailbox -> mailboxesShow auth domain mailbox
    MailboxesCreate domain mailbox -> mailboxesCreate auth domain mailbox
    MailboxesDelete domain localPart -> mailboxesDelete auth domain localPart
    IdentitiesIndex domain localPart -> identitiesIndex auth domain localPart
    IdentitiesCreate domain aliasTo identity -> identitiesCreate auth domain aliasTo identity
    IdentitiesUpdate domain aliasTo localPart identity -> identitiesUpdate auth domain aliasTo localPart identity
