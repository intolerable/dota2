module WebAPI.Dota.Types.Team
  ( Team(Team)
  , Faction(..)
  , SF.HasName(..)
  , SF.HasIdentifier(..)
  , SF.HasLogo(..)
  , SF.HasComplete(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Prelude

data Faction = Radiant | Dire
  deriving (Show, Read, Eq)

data Team =
  Team { _teamName :: Maybe Text
       , _teamIdentifier :: Maybe Integer
       , _teamLogo :: Maybe Integer
       , _teamComplete :: Maybe Bool }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Team where
  parseJSON (Object o) =
    Team <$> o .:? "team_name"
         <*> o .:? "team_id"
         <*> o .:? "team_logo"
         <*> o .:? "complete"
  parseJSON _ = fail "Team parse failed"

makeFields ''Team
