{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fields where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Prelude hiding (Read)
import Data.Kind (Type, Constraint)

type data MailboxType = Create | Read | Update

type All (c :: Type -> Constraint) (typ :: MailboxType) =
  ( c (LocalPart typ)
  , c (DomainName typ)
  , c (Address typ)
  , c (Password typ)
  )

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

type family Password (typ :: MailboxType) where
  Password Create = Text
  Password Read = ()
  Password Update = Text

data PasswordMethod (typ :: MailboxType) = Invitation Text | Password (Password typ)

deriving stock instance Eq (Password typ) => Eq (PasswordMethod typ)
deriving stock instance Ord (Password typ) => Ord (PasswordMethod typ)
deriving stock instance Show (Password typ) => Show (PasswordMethod typ)

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
