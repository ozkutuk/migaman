{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Migadu
  ( module Migadu
  , module Migadu.Fields
  , module Migadu.Identity
  , module Migadu.Mailbox
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as Req
import System.Environment qualified as Env
import Prelude hiding (Read)

import Migadu.Fields (MailboxType (..))
import Migadu.Identity (Identities (..), Identity (..))
import Migadu.Mailbox (Mailbox, Mailboxes)

baseEndpoint :: Req.Url 'Req.Https
baseEndpoint = Req.https "api.migadu.com" /: "v1"

data MigaduAuth = MigaduAuth
  { account :: ByteString
  , key :: ByteString
  }

getAuth :: IO (Maybe MigaduAuth)
getAuth = do
  mAccount <- fmap BSC.pack <$> Env.lookupEnv "MIGADU_ACCOUNT"
  mKey <- fmap BSC.pack <$> Env.lookupEnv "MIGADU_API_KEY"
  pure $ MigaduAuth <$> mAccount <*> mKey

data MigaduRequest a where
  MailboxesIndex :: Text -> MigaduRequest (Mailboxes Read)
  MailboxesShow :: Text -> Text -> MigaduRequest (Mailbox Read)
  -- MailboxesShow :: Text -> Text -> MigaduRequest Aeson.Value
  MailboxesCreate :: Text -> Mailbox Create -> MigaduRequest (Mailbox Read)
  MailboxesDelete :: Text -> Text -> MigaduRequest (Mailbox Read)
  IdentitiesIndex :: Text -> Text -> MigaduRequest (Identities Read)

mkAuthOpts :: MigaduAuth -> Req.Option 'Req.Https
mkAuthOpts (MigaduAuth account key) = Req.basicAuth account key

mailboxesIndex :: MigaduAuth -> Text -> Req.Req (Mailboxes Read)
mailboxesIndex auth domain =
  Req.responseBody
    <$> Req.req
      Req.GET
      (baseEndpoint /: "domains" /: domain /: "mailboxes")
      Req.NoReqBody
      Req.jsonResponse
      (mkAuthOpts auth)

mailboxesShow :: MigaduAuth -> Text -> Text -> Req.Req (Mailbox Read)
-- mailboxesShow :: MigaduAuth -> Text -> Text -> Req.Req Aeson.Value
mailboxesShow auth domain mailbox =
  Req.responseBody
    <$> Req.req
      Req.GET
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: mailbox)
      Req.NoReqBody
      Req.jsonResponse
      (mkAuthOpts auth)

mailboxesCreate :: MigaduAuth -> Text -> Mailbox Create -> Req.Req (Mailbox Read)
mailboxesCreate auth domain mailbox =
  Req.responseBody
    <$> Req.req
      Req.POST
      (baseEndpoint /: "domains" /: domain /: "mailboxes")
      (Req.ReqBodyJson mailbox)
      Req.jsonResponse
      (mkAuthOpts auth)

mailboxesDelete :: MigaduAuth -> Text -> Text -> Req.Req (Mailbox Read)
mailboxesDelete auth domain localPart =
  Req.responseBody
    <$> Req.req
      Req.DELETE
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: localPart)
      Req.NoReqBody
      Req.jsonResponse
      (mkAuthOpts auth)

identitiesIndex :: MigaduAuth -> Text -> Text -> Req.Req (Identities Read)
identitiesIndex auth domain localPart =
  Req.responseBody
    <$> Req.req
      Req.GET
      (baseEndpoint /: "domains" /: domain /: "mailboxes" /: localPart /: "identities")
      Req.NoReqBody
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
