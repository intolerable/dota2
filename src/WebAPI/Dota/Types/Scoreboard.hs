module WebAPI.Dota.Types.Scoreboard
  ( Scoreboard(Scoreboard)
  , SF.HasDuration(..)
  , SF.HasRoshTimer(..)
  , SF.HasRadiantTeam(..)
  , SF.HasDireTeam(..)
  , SF.HasScore(..)
  , SF.HasTowerState(..)
  , SF.HasBarracksState(..)
  , SF.HasPicks(..)
  , SF.HasBans(..)
  , SF.HasPlayers(..)
  , SF.HasAbilities(..) )where

import WebAPI.Dota.Internal.SharedFields as SF
import WebAPI.Dota.Types.Ability
import WebAPI.Dota.Types.Hero
import WebAPI.Dota.Types.Player

import Control.Lens.TH
import Data.Aeson

data Scoreboard =
  Scoreboard { _scoreboardDuration :: Double
             , _scoreboardRoshTimer :: Maybe Integer
             , _scoreboardRadiantTeam :: Maybe TeamScore
             , _scoreboardDireTeam :: Maybe TeamScore }
  deriving (Show, Eq)

instance FromJSON Scoreboard where
  parseJSON (Object o) =
    Scoreboard <$> o .: "duration"
               <*> o .:? "rosh_respawn_timer"
               <*> o .:? "radiant"
               <*> o .:? "dire"
  parseJSON _ = fail "Scoreboard parse failed"

data TeamScore =
  TeamScore { _teamScoreScore :: Integer
            , _teamScoreTowerState :: Integer
            , _teamScoreBarracksState :: Integer
            , _teamScorePicks :: Maybe [HeroID]
            , _teamScoreBans :: Maybe [HeroID]
            , _teamScorePlayers :: [Player]
            , _teamScoreAbilities :: Maybe [Ability] }
  deriving (Show, Eq)

instance FromJSON TeamScore where
  parseJSON (Object o) =
    TeamScore <$> o .: "score"
              <*> o .: "tower_state"
              <*> o .: "barracks_state"
              <*> o .:? "picks"
              <*> o .:? "bans"
              <*> o .: "players"
              <*> o .:? "abilities"
  parseJSON _ = fail "TeamScore parse failed"

makeFields ''Scoreboard
makeFields ''TeamScore
