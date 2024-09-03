{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Migadu where

import Data.Aeson (FromJSON (..), ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as Req
import System.Environment qualified as Env
import Prelude hiding (Read)

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

mkAuthOpts :: MigaduAuth -> Req.Option 'Req.Https
mkAuthOpts (MigaduAuth account key) = Req.basicAuth account key

data PasswordMethod (typ :: MailboxType) = Invitation Text | Password (Password typ)

deriving stock instance Eq (Password typ) => Eq (PasswordMethod typ)
deriving stock instance Ord (Password typ) => Ord (PasswordMethod typ)
deriving stock instance Show (Password typ) => Show (PasswordMethod typ)

type family Password (typ :: MailboxType) where
  Password Create = Text
  Password Read = ()
  Password Update = Text

data SpamAction = Folder
  deriving stock (Eq, Ord, Show)

instance ToJSON SpamAction where
  toJSON :: SpamAction -> Aeson.Value
  toJSON Folder = Aeson.String "folder"

instance FromJSON SpamAction where
  parseJSON = Aeson.withText "SpamAction" $ \case
    "folder" -> pure Folder
    _ -> fail "Invalid SpamAction"

data SpamAggressiveness = DefaultAggressiveness
  deriving stock (Eq, Ord, Show)

instance FromJSON SpamAggressiveness where
  parseJSON = Aeson.withText "SpamAggressiveness" $ \case
    "default" -> pure DefaultAggressiveness
    _ -> fail "Invalid SpamAggressiveness"

instance ToJSON SpamAggressiveness where
  toJSON :: SpamAggressiveness -> Aeson.Value
  toJSON DefaultAggressiveness = Aeson.String "default"

newtype Mailboxes (typ :: MailboxType) = Mailboxes {mailboxes :: [Mailbox typ]}
  deriving stock (Generic)

deriving stock instance All Eq typ => Eq (Mailboxes typ)
deriving stock instance All Ord typ => Ord (Mailboxes typ)
deriving stock instance All Show typ => Show (Mailboxes typ)

instance FromJSON (Mailboxes Read) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Mailboxes Read)
  parseJSON = Aeson.genericParseJSON aesonOptions

type data MailboxType = Create | Read | Update

type family LocalPart (typ :: MailboxType) where
  LocalPart Create = Text
  LocalPart Read = Text
  LocalPart Update = ()

type family DomainName (typ :: MailboxType) where
  DomainName Create = ()
  DomainName Read = Text
  DomainName Update = ()

type family Address (typ :: MailboxType) where
  Address Create = ()
  Address Read = Text
  Address Update = ()

data Mailbox (typ :: MailboxType) = Mailbox
  { localPart :: !(LocalPart typ) -- CREATE/READ ONLY
  -- TODO(ozkutuk): docs say the field name is "domain", but response
  -- seems to be "domain_name"? Clarify.
  , domainName :: !(DomainName typ) -- READ ONLY
  , address :: !(Address typ) -- READ ONLY
  , name :: Text
  , isInternal :: Bool
  , maySend :: Bool
  , mayReceive :: Bool
  , mayAccessImap :: Bool
  , mayAccessPop3 :: Bool
  , mayAccessManagesieve :: Bool
  , passwordMethod :: !(PasswordMethod typ)
  , -- , password -- CREATE/UPDATE ONLY
    -- TODO(ozkutuk): docs say the field type is String, but response
    -- seems to be nullable
    -- passwordRecoveryEmail :: Maybe Text
    spamAction :: SpamAction
  , spamAggressiveness :: SpamAggressiveness
  , -- TODO(ozkutuk): docs say these are string fields, but response
    -- seems to be lists? Clarify.
    senderDenylist :: [Text]
  , senderAllowlist :: [Text]
  , recipientDenylist :: [Text]
  , autorespondActive :: Bool
  , autorespondSubject :: Text
  , autorespondBody :: Text
  , -- TODO(ozkutuk): docs say the field type is Date, but response
    -- seems to be nullable
    autorespondExpiresOn :: Maybe UTCTime
  , -- TODO(ozkutuk): these footer fields are not present
    footerActive :: Maybe Text
  , footerPlainBody :: Maybe Text
  , footerHtmlBody :: Maybe Text
  }
  deriving stock (Generic)

deriving stock instance All Eq typ => Eq (Mailbox typ)
deriving stock instance All Ord typ => Ord (Mailbox typ)
deriving stock instance All Show typ => Show (Mailbox typ)

type All (c :: Type -> Constraint) (typ :: MailboxType) =
  ( c (LocalPart typ)
  , c (DomainName typ)
  , c (Address typ)
  , c (Password typ)
  )

instance FromJSON (Mailbox Read) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Mailbox Read)
  parseJSON = Aeson.withObject "Mailbox" $ \v -> do
    Mailbox
      <$> v .: "local_part"
      <*> v .: "domain_name"
      <*> v .: "address"
      <*> v .: "name"
      <*> v .: "is_internal"
      <*> v .: "may_send"
      <*> v .: "may_receive"
      <*> v .: "may_access_imap"
      <*> v .: "may_access_pop3"
      <*> v .: "may_access_managesieve"
      <*> passwordMethodParser v
      <*> v .: "spam_action"
      <*> v .: "spam_aggressiveness"
      <*> v .: "sender_denylist"
      <*> v .: "sender_allowlist"
      <*> v .: "recipient_denylist"
      <*> v .: "autorespond_active"
      <*> v .: "autorespond_subject"
      <*> v .: "autorespond_body"
      <*> v .: "autorespond_expires_on"
      <*> v .:? "footer_active"
      <*> v .:? "footer_plain_body"
      <*> v .:? "footer_html_body"
    where
      passwordMethodParser :: Aeson.Object -> Aeson.Parser (PasswordMethod Read)
      passwordMethodParser v = maybe (Password ()) Invitation <$> v .:? "password_recovery_email"

appendObject :: Aeson.Value -> Aeson.Value -> Aeson.Value
appendObject (Aeson.Object o1) (Aeson.Object o2) = Aeson.Object (o2 <> o1)
appendObject v _ = v

instance ToJSON (Mailbox Create) where
  toJSON :: Mailbox Create -> Aeson.Value
  toJSON mailbox =
    appendObject (passwordFields mailbox.passwordMethod) $
      Aeson.Object $
        "local_part" .= mailbox.localPart
          <> "domain_name" .= mailbox.domainName
          <> "address" .= mailbox.address
          <> "name" .= mailbox.name
          <> "is_internal" .= mailbox.isInternal
          <> "may_send" .= mailbox.maySend
          <> "may_receive" .= mailbox.mayReceive
          <> "may_access_imap" .= mailbox.mayAccessImap
          <> "may_access_pop3" .= mailbox.mayAccessPop3
          <> "may_access_managesieve" .= mailbox.mayAccessManagesieve
          <> "spam_action" .= mailbox.spamAction
          <> "spam_aggressiveness" .= mailbox.spamAggressiveness
          <> "sender_denylist" .= mailbox.senderDenylist
          <> "sender_allowlist" .= mailbox.senderAllowlist
          <> "recipient_denylist" .= mailbox.recipientDenylist
          <> "autorespond_active" .= mailbox.autorespondActive
          <> "autorespond_subject" .= mailbox.autorespondSubject
          <> "autorespond_body" .= mailbox.autorespondBody
          <> "autorespond_expires_on" .= mailbox.autorespondExpiresOn
          <> "footer_active" .= mailbox.footerActive
          <> "footer_plain_body" .= mailbox.footerPlainBody
          <> "footer_html_body" .= mailbox.footerHtmlBody
    where
      passwordFields :: PasswordMethod Create -> Aeson.Value
      passwordFields (Invitation recoveryEmail) =
        Aeson.Object $
          "password_method" .= ("invitation" :: Text)
            <> "password_recovery_email" .= recoveryEmail
      passwordFields (Password password) =
        Aeson.Object $
          "password_method" .= ("password" :: Text)
            <> "password" .= password

-- readBox :: Mailbox Read
-- readBox =
--   Mailbox
--     { localPart = "foo"
--     , name = "bar"
--     , domainName = "foobar"
--     , address = "foo@foobar"
--     , isInternal = False
--     , maySend = True
--     , mayReceive = True
--     , mayAccessImap = True
--     , mayAccessPop3 = True
--     , mayAccessManagesieve = True
--     , passwordMethod = Password ()
--     , -- , passwordRecoveryEmail = Nothing
--       spamAction = Folder
--     , spamAggressiveness = DefaultAggressiveness
--     , senderDenylist = []
--     , senderAllowlist = []
--     , recipientDenylist = []
--     , autorespondActive = False
--     , autorespondSubject = T.empty
--     , autorespondBody = T.empty
--     , autorespondExpiresOn = Nothing
--     , footerActive = Nothing
--     , footerPlainBody = Nothing
--     , footerHtmlBody = Nothing
--     }

mkMailbox :: Text -> Text -> Text -> Mailbox Create
mkMailbox name localPart password =
  Mailbox
    { localPart
    , name
    , domainName = ()
    , address = ()
    , isInternal = False
    , maySend = True
    , mayReceive = True
    , mayAccessImap = True
    , mayAccessPop3 = True
    , mayAccessManagesieve = True
    , passwordMethod = Password password
    , spamAction = Folder
    , spamAggressiveness = DefaultAggressiveness
    , senderDenylist = []
    , senderAllowlist = []
    , recipientDenylist = []
    , autorespondActive = False
    , autorespondSubject = T.empty
    , autorespondBody = T.empty
    , autorespondExpiresOn = Nothing
    , footerActive = Nothing
    , footerPlainBody = Nothing
    , footerHtmlBody = Nothing
    }

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

aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    , Aeson.omitNothingFields = True
    }

runMigadu :: MigaduAuth -> MigaduRequest a -> IO a
runMigadu auth =
  Req.runReq Req.defaultHttpConfig . \case
    MailboxesIndex domain -> mailboxesIndex auth domain
    MailboxesShow domain mailbox -> mailboxesShow auth domain mailbox
    MailboxesCreate domain mailbox -> mailboxesCreate auth domain mailbox
