module WebAPI.Dota.Types.Scoreboard
  ( LiveScoreboard(LiveScoreboard)
  , LiveTeamScore(LiveTeamScore)
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

import Control.Applicative
import Control.Lens.TH
import Data.Aeson
import Prelude

data LiveScoreboard =
  LiveScoreboard { _liveScoreboardDuration :: Integer
                 , _liveScoreboardRoshTimer :: Maybe Integer
                 , _liveScoreboardRadiantTeam :: Maybe LiveTeamScore
                 , _liveScoreboardDireTeam :: Maybe LiveTeamScore }
  deriving (Show, Eq)

instance FromJSON LiveScoreboard where
  parseJSON (Object o) =
    LiveScoreboard <$> o .: "duration"
                 <*> o .:? "rosh_respawn_timer"
                 <*> o .:? "radiant"
                 <*> o .:? "dire"
  parseJSON _ = fail "Scoreboard parse failed"

data LiveTeamScore =
  LiveTeamScore { _liveTeamScoreScore :: Integer
                , _liveTeamScoreTowerState :: Integer
                , _liveTeamScoreBarracksState :: Integer
                , _liveTeamScorePicks :: Maybe [HeroID]
                , _liveTeamScoreBans :: Maybe [HeroID]
                , _liveTeamScorePlayers :: [LivePlayer]
                , _liveTeamScoreAbilities :: Maybe [Ability] }
  deriving (Show, Eq)

instance FromJSON LiveTeamScore where
  parseJSON (Object o) =
    LiveTeamScore <$> o .: "score"
                  <*> o .: "tower_state"
                  <*> o .: "barracks_state"
                  <*> o .:? "picks"
                  <*> o .:? "bans"
                  <*> o .: "players"
                  <*> o .:? "abilities"
  parseJSON _ = fail "LiveTeamScore parse failed"

makeFields ''LiveScoreboard
makeFields ''LiveTeamScore
