{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Migadu.Identity where

import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Migadu.Fields (Address, All, AllUpdateable, DomainName, LocalPart, MailboxType (..), Updateable)
import Prelude hiding (Read)

newtype Identities (typ :: MailboxType) = Identities {identities :: [Identity typ]}
  deriving stock (Generic)

deriving stock instance (All Eq typ, AllUpdateable Eq typ '[Text, Bool]) => Eq (Identities typ)
deriving stock instance (All Ord typ, AllUpdateable Ord typ '[Text, Bool]) => Ord (Identities typ)
deriving stock instance (All Show typ, AllUpdateable Show typ '[Text, Bool]) => Show (Identities typ)

instance FromJSON (Identities 'Read) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Identities 'Read)
  parseJSON = Aeson.genericParseJSON aesonOptions

-- Maybe this should carry the "host mailbox" information as well?
data Identity (typ :: MailboxType) = Identity
  { localPart :: !(LocalPart typ)
  , domainName :: !(DomainName typ)
  , address :: !(Address typ)
  , name :: !(Updateable typ Text)
  , maySend :: !(Updateable typ Bool)
  , mayReceive :: !(Updateable typ Bool)
  , mayAccessImap :: !(Updateable typ Bool)
  , mayAccessPop3 :: !(Updateable typ Bool)
  , mayAccessManagesieve :: !(Updateable typ Bool)
  , footerActive :: Maybe Text
  , footerPlainBody :: Maybe Text
  , footerHtmlBody :: Maybe Text
  }
  deriving stock (Generic)

deriving stock instance (All Eq typ, AllUpdateable Eq typ '[Text, Bool]) => Eq (Identity typ)
deriving stock instance (All Ord typ, AllUpdateable Ord typ '[Text, Bool]) => Ord (Identity typ)
deriving stock instance (All Show typ, AllUpdateable Show typ '[Text, Bool]) => Show (Identity typ)

instance FromJSON (Identity 'Read) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Identity 'Read)
  parseJSON = Aeson.withObject "Identity" $ \v -> do
    Identity
      <$> v .: "local_part"
      <*> v .: "domain_name"
      <*> v .: "address"
      <*> v .: "name"
      <*> v .: "may_send"
      <*> v .: "may_receive"
      <*> v .: "may_access_imap"
      <*> v .: "may_access_pop3"
      <*> v .: "may_access_managesieve"
      <*> v .:? "footer_active"
      <*> v .:? "footer_plain_body"
      <*> v .:? "footer_html_body"

appendObject :: Aeson.Value -> Aeson.Value -> Aeson.Value
appendObject (Aeson.Object o1) (Aeson.Object o2) = Aeson.Object (o2 <> o1)
appendObject v _ = v

instance ToJSON (Identity 'Create) where
  toJSON :: Identity 'Create -> Aeson.Value
  toJSON identity =
    Aeson.Object $
      "local_part" .= identity.localPart
        <> "domain_name" .= identity.domainName
        <> "address" .= identity.address
        <> "name" .= identity.name
        <> "may_send" .= identity.maySend
        <> "may_receive" .= identity.mayReceive
        <> "may_access_imap" .= identity.mayAccessImap
        <> "may_access_pop3" .= identity.mayAccessPop3
        <> "may_access_managesieve" .= identity.mayAccessManagesieve
        <> "footer_active" .= identity.footerActive
        <> "footer_plain_body" .= identity.footerPlainBody
        <> "footer_html_body" .= identity.footerHtmlBody

defaultCreateIdentity :: Text -> Text -> Identity 'Create
defaultCreateIdentity name localPart =
  Identity
    { localPart
    , name
    , domainName = ()
    , address = ()
    , maySend = True
    , mayReceive = True
    , mayAccessImap = True
    , mayAccessPop3 = True
    , mayAccessManagesieve = True
    , footerActive = Nothing
    , footerPlainBody = Nothing
    , footerHtmlBody = Nothing
    }

instance ToJSON (Identity 'Update) where
  toJSON :: Identity 'Update -> Aeson.Value
  toJSON identity =
    Aeson.Object $
      maybeField "name" identity.name
        <> maybeField "may_send" identity.maySend
        <> maybeField "may_receive" identity.mayReceive
        <> maybeField "may_access_imap" identity.mayAccessImap
        <> maybeField "may_access_pop3" identity.mayAccessPop3
        <> maybeField "may_access_managesieve" identity.mayAccessManagesieve
        <> maybeField "footer_active" identity.footerActive
        <> maybeField "footer_plain_body" identity.footerPlainBody
        <> maybeField "footer_html_body" identity.footerHtmlBody

maybeField :: Aeson.ToJSON a => Aeson.Key -> Maybe a -> Aeson.Object
maybeField k = maybe mempty (k .=)

defaultUpdateIdentity :: Identity 'Update
defaultUpdateIdentity =
  Identity
    { localPart = ()
    , domainName = ()
    , address = ()
    , name = Nothing
    , maySend = Nothing
    , mayReceive = Nothing
    , mayAccessImap = Nothing
    , mayAccessPop3 = Nothing
    , mayAccessManagesieve = Nothing
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
