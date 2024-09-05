{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Mailbox where

import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Fields (Address, All, DomainName, LocalPart, MailboxType (..), PasswordMethod (..), SpamAction (..), SpamAggressiveness (..))
import GHC.Generics (Generic)
import Prelude hiding (Read)

newtype Mailboxes (typ :: MailboxType) = Mailboxes {mailboxes :: [Mailbox typ]}
  deriving stock (Generic)

deriving stock instance All Eq typ => Eq (Mailboxes typ)
deriving stock instance All Ord typ => Ord (Mailboxes typ)
deriving stock instance All Show typ => Show (Mailboxes typ)

instance FromJSON (Mailboxes Read) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Mailboxes Read)
  parseJSON = Aeson.genericParseJSON aesonOptions

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

aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    , Aeson.omitNothingFields = True
    }
