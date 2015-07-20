module WebAPI.Dota.Types.Match
  ( LiveMatchListing(LiveMatchListing)
  , LiveLeagueMatch(LiveLeagueMatch)
  , MatchID(..)
  , HasSeries
  , gameOfSeries
  , SF.HasMatches(..)
  , SF.HasIdentifier(..)
  , SF.HasPlayers(..)
  , SF.HasRadiantTeam(..)
  , SF.HasDireTeam(..)
  , SF.HasRadiantSeriesWins(..)
  , SF.HasDireSeriesWins(..)
  , SF.HasSeriesType(..)
  , SF.HasLobbyID(..)
  , SF.HasSpectators(..)
  , SF.HasStreamDelay(..)
  , SF.HasLeagueID(..)
  , SF.HasLeagueTier(..)
  , SF.HasScoreboard(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF
import WebAPI.Dota.Types.League
import WebAPI.Dota.Types.Player
import WebAPI.Dota.Types.Team
import WebAPI.Dota.Types.Scoreboard

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
                  , _liveLeagueMatchPlayers :: [PlayerAccount]
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
                  , _liveLeagueMatchScoreboard :: Maybe Scoreboard }
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
  parseJSON _ = fail "Match parse failed"

data Match =
  Match { _matchWinner :: Bool
        , _matchDuration :: Integer
        , _matchStartTime :: Integer }
  deriving (Show, Read, Eq)

type HasSeries s a =
  ( HasRadiantSeriesWins s a
  , HasDireSeriesWins s a
  , HasSeriesType s a )

gameOfSeries :: (Num a, HasSeries s a) => Getter s a
gameOfSeries = to $ \x -> 1 + (x^.radiantSeriesWins) + (x^.direSeriesWins)

makeFields ''LiveLeagueMatch
makeFields ''LiveMatchListing
