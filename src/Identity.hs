{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Identity where

import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Fields (Address, All, DomainName, LocalPart, MailboxType (..))
import GHC.Generics (Generic)
import Prelude hiding (Read)

newtype Identities (typ :: MailboxType) = Identities {identities :: [Identity typ]}
  deriving stock (Generic)

deriving stock instance All Eq typ => Eq (Identities typ)
deriving stock instance All Ord typ => Ord (Identities typ)
deriving stock instance All Show typ => Show (Identities typ)

instance FromJSON (Identities Read) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Identities Read)
  parseJSON = Aeson.genericParseJSON aesonOptions

data Identity (typ :: MailboxType) = Identity
  { localPart :: !(LocalPart typ)
  , domainName :: !(DomainName typ)
  , address :: !(Address typ)
  , name :: Text
  , maySend :: Bool
  , mayReceive :: Bool
  , mayAccessImap :: Bool
  , mayAccessPop3 :: Bool
  , mayAccessManagesieve :: Bool
  , -- , password :: !(PasswordMethod typ)
    footerActive :: Maybe Text
  , footerPlainBody :: Maybe Text
  , footerHtmlBody :: Maybe Text
  }
  deriving stock (Generic)

deriving stock instance All Eq typ => Eq (Identity typ)
deriving stock instance All Ord typ => Ord (Identity typ)
deriving stock instance All Show typ => Show (Identity typ)

instance FromJSON (Identity Read) where
  parseJSON :: Aeson.Value -> Aeson.Parser (Identity Read)
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
      -- <*> passwordMethodParser v
      <*> v .:? "footer_active"
      <*> v .:? "footer_plain_body"
      <*> v .:? "footer_html_body"

-- where
--   passwordMethodParser :: Aeson.Object -> Aeson.Parser (PasswordMethod Read)
--   passwordMethodParser v = maybe (Password ()) Invitation <$> v .:? "password_recovery_email"

appendObject :: Aeson.Value -> Aeson.Value -> Aeson.Value
appendObject (Aeson.Object o1) (Aeson.Object o2) = Aeson.Object (o2 <> o1)
appendObject v _ = v

instance ToJSON (Identity Create) where
  toJSON :: Identity Create -> Aeson.Value
  toJSON identity =
    -- appendObject (passwordFields identity.passwordMethod) $
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

--     where
--       passwordFields :: PasswordMethod Create -> Aeson.Value
--       passwordFields (Invitation recoveryEmail) =
--         Aeson.Object $
--           "password_method" .= ("invitation" :: Text)
--             <> "password_recovery_email" .= recoveryEmail
--       passwordFields (Password password) =
--         Aeson.Object $
--           "password_method" .= ("password" :: Text)
--             <> "password" .= password
--

aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_'
    , Aeson.omitNothingFields = True
    }
