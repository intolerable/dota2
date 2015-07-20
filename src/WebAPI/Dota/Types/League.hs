module WebAPI.Dota.Types.League
  ( League(League)
  , LeagueID(..)
  , LeagueListing(LeagueListing)
  , PrizePool(PrizePool)
  , LeagueTier(..)
  , SF.HasLeagues(..)
  , SF.HasName(..)
  , SF.HasIdentifier(..)
  , SF.HasDescription(..)
  , SF.HasUrl(..)
  , SF.HasItemdef(..)
  , SF.HasLeagueID(..)
  , SF.HasAmount(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF

import Control.Applicative
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable
import Data.Text (Text)
import Network.API.Builder
import Prelude

newtype LeagueID = LeagueID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON LeagueID where
  parseJSON (Object o) = LeagueID <$> o .: "league_id"
  parseJSON x = asum
    [ LeagueID <$> parseJSON x
    , fail "LeagueID parse failed" ]

instance ToQuery LeagueID where
  toQuery k (LeagueID v) = toQuery k v

newtype LeagueListing = LeagueListing {
  _leagueListingLeagues :: [League] }
  deriving (Show)

instance FromJSON LeagueListing where
  parseJSON (Object o) = LeagueListing <$> o .: "leagues"
  parseJSON _ = fail "LeagueListing parse failed"

data League =
  League { _leagueName :: Text
         , _leagueIdentifier :: LeagueID
         , _leagueDescription :: Text
         , _leagueUrl :: Text
         , _leagueItemdef :: Integer }
  deriving (Show, Eq)

instance FromJSON League where
  parseJSON (Object o) =
    League <$> o .: "name"
           <*> o .: "leagueid"
           <*> o .: "description"
           <*> o .: "tournament_url"
           <*> o .: "itemdef"
  parseJSON _ = fail "League parse failed"

data PrizePool =
  PrizePool { _prizePoolLeagueID :: LeagueID
            , _prizePoolAmount :: Integer }
  deriving (Show, Read, Eq)

instance FromJSON PrizePool where
  parseJSON (Object o) =
    PrizePool <$> o .: "league_id"
              <*> o .: "prize_pool"
  parseJSON _ = fail "PrizePool parse failed"

data LeagueTier = Unknown
                | Amateur
                | Professional
                | Premium
  deriving (Show, Read, Eq, Ord)

instance FromJSON LeagueTier where
  parseJSON x = asum
    [ (parseJSON x :: Parser Integer) >>= \case
        3 -> pure Premium
        2 -> pure Professional
        1 -> pure Amateur
        0 -> pure Unknown
        _ -> fail "Unknown LeagueTier"
    , fail "LeagueTier parse failed" ]

makeFields ''League
makeFields ''LeagueListing
makeFields ''PrizePool
