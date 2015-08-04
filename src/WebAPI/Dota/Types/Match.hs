module WebAPI.Dota.Types.Match
  ( LiveMatchListing(LiveMatchListing)
  , LiveLeagueMatch(LiveLeagueMatch)
  , MatchID(..)
  , Match(Match)
  , HasSeries
  , PickBan(PickBan)
  , gameOfSeries
  , SF.HasDireSeriesWins(..)
  , SF.HasDireTeam(..)
  , SF.HasDraft(..)
  , SF.HasDuration(..)
  , SF.HasFaction(..)
  , SF.HasHeroID(..)
  , SF.HasIdentifier(..)
  , SF.HasIsPick(..)
  , SF.HasLeagueID(..)
  , SF.HasLeagueTier(..)
  , SF.HasLobbyID(..)
  , SF.HasMatches(..)
  , SF.HasOrder(..)
  , SF.HasPlayers(..)
  , SF.HasRadiantSeriesWins(..)
  , SF.HasRadiantTeam(..)
  , SF.HasScoreboard(..)
  , SF.HasSeriesType(..)
  , SF.HasSpectators(..)
  , SF.HasStreamDelay(..)
  , SF.HasWinner(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF
import WebAPI.Dota.Types.Hero
import WebAPI.Dota.Types.League
import WebAPI.Dota.Types.Player
import WebAPI.Dota.Types.Scoreboard
import WebAPI.Dota.Types.Team

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Foldable
import Network.API.Builder
import Prelude

newtype LiveMatchListing = LiveMatchListing { _liveMatchListingMatches :: [LiveLeagueMatch] }
  deriving (Show, Eq)

instance FromJSON LiveMatchListing where
  parseJSON (Object o) = LiveMatchListing <$> o .: "games"
  parseJSON _ = fail "LiveMatchListing parse failed"

newtype MatchID = MatchID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON MatchID where
  parseJSON (Object o) = MatchID <$> o .: "match_id"
  parseJSON x = asum
    [ MatchID <$> parseJSON x
    , fail "MatchID parse failed" ]

instance ToQuery MatchID where
  toQuery k (MatchID v) = toQuery k v

data LiveLeagueMatch =
  LiveLeagueMatch { _liveLeagueMatchIdentifier :: MatchID
                  , _liveLeagueMatchPlayers :: [LivePlayerAccount]
                  , _liveLeagueMatchRadiantTeam :: Maybe Team
                  , _liveLeagueMatchDireTeam :: Maybe Team
                  , _liveLeagueMatchRadiantSeriesWins :: Integer
                  , _liveLeagueMatchDireSeriesWins :: Integer
                  , _liveLeagueMatchSeriesType :: Integer
                  , _liveLeagueMatchLobbyID :: Integer
                  , _liveLeagueMatchSpectators :: Integer
                  , _liveLeagueMatchStreamDelay :: Integer
                  , _liveLeagueMatchLeagueID :: LeagueID
                  , _liveLeagueMatchLeagueTier :: LeagueTier
                  , _liveLeagueMatchScoreboard :: Maybe LiveScoreboard }
  deriving (Show, Eq)

instance FromJSON LiveLeagueMatch where
  parseJSON (Object o) =
    LiveLeagueMatch <$> o .: "match_id"
                    <*> o .: "players"
                    <*> o .:? "radiant_team"
                    <*> o .:? "dire_team"
                    <*> o .: "radiant_series_wins"
                    <*> o .: "dire_series_wins"
                    <*> o .: "series_type"
                    <*> o .: "lobby_id"
                    <*> o .: "spectators"
                    <*> o .: "stream_delay_s"
                    <*> o .: "league_id"
                    <*> o .: "league_tier"
                    <*> o .:? "scoreboard"
  parseJSON _ = fail "LiveLeagueMatch parse failed"

data Match =
  Match { _matchIdentifier :: MatchID
        , _matchWinner :: Faction
        , _matchDuration :: Integer
        , _matchStartTime :: Integer
        , _matchRadiantTeam :: Team
        , _matchDireTeam :: Team
        , _matchPlayers :: [Player]
        , _matchDraft :: [PickBan] }
  deriving (Show, Read, Eq)

instance FromJSON Match where
  parseJSON (Object o) =
    Match <$> o .: "match_id"
          <*> (if_ Radiant Dire <$> o .: "radiant_win")
          <*> o .: "duration"
          <*> o .: "start_time"
          <*> (Team <$> o .:? "radiant_name"
                    <*> o .:? "radiant_team_id"
                    <*> o .:? "radiant_logo"
                    <*> pure Nothing)
          <*> (Team <$> o .:? "dire_name"
                    <*> o .:? "dire_team_id"
                    <*> o .:? "dire_logo"
                    <*> pure Nothing)
          <*> o .: "players"
          <*> o .: "picks_bans"
  parseJSON _ = fail "Match parse failed"

data PickBan =
  PickBan { _pickBanIsPick :: Bool
          , _pickBanHeroID :: HeroID
          , _pickBanFaction :: Faction
          , _pickBanOrder :: Integer }
  deriving (Show, Read, Eq)

instance FromJSON PickBan where
  parseJSON (Object o) =
    PickBan <$> o .: "is_pick"
            <*> o .: "hero_id"
            <*> (factionID =<< o .: "team")
            <*> o .: "order"
  parseJSON _ = fail "PickBan parse failed"

if_ :: a -> a -> Bool -> a
if_ t f b = if b then t else f

factionID :: Monad m => Int -> m Faction
factionID 0 = return Radiant
factionID 1 = return Dire
factionID _ = fail "Invalid faction ID"

type HasSeries s a =
  ( HasRadiantSeriesWins s a
  , HasDireSeriesWins s a
  , HasSeriesType s a )

gameOfSeries :: (Num a, HasSeries s a) => Getter s a
gameOfSeries = to $ \x -> 1 + (x^.radiantSeriesWins) + (x^.direSeriesWins)

makeFields ''LiveLeagueMatch
makeFields ''LiveMatchListing
makeFields ''Match
makeFields ''PickBan
