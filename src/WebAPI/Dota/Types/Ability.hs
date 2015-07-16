module WebAPI.Dota.Types.Ability
  ( Ability(Ability)
  , AbilityID(..)
  , SF.HasIdentifier(..)
  , SF.HasLevel(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF

import Control.Lens.TH
import Data.Aeson
import Data.Foldable

data Ability =
  Ability { _abilityIdentifier :: AbilityID
          , _abilityLevel :: Integer }
  deriving (Show, Eq)

instance FromJSON Ability where
  parseJSON (Object o) =
    Ability <$> o .: "ability_id"
            <*> o .: "ability_level"
  parseJSON _ = fail "Ability parse failed"

newtype AbilityID = AbilityID Integer
  deriving (Show, Read, Eq)

instance FromJSON AbilityID where
  parseJSON (Object o) = AbilityID <$> o .: "ability_id"
  parseJSON x = asum
    [ AbilityID <$> parseJSON x
    , fail "AbilityID parse failed" ]

makeFields ''Ability
