module WebAPI.Dota where

import WebAPI.Dota.Internal.SharedFields
import WebAPI.Dota.Types.Hero
import WebAPI.Dota.Types.Item
import WebAPI.Dota.Types.League
import WebAPI.Dota.Types.Match
import WebAPI.Dota.Types.Player
import WebAPI.Dota.Types.Result

import Control.Applicative
import Control.Lens hiding (coerce)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Coerce
import Data.Map (Map)
import Data.Text (Text)
import Network.API.Builder hiding (name)
import qualified Data.Map as Map

newtype WebAPIT m a = WebAPIT (APIT (WebAPIKey, Language) () m a)
  deriving (Functor, Applicative, Alternative, Monad, MonadIO)

type WebAPI = WebAPIT IO

newtype WebAPIKey = WebAPIKey Text
  deriving (Show)

instance ToQuery WebAPIKey where
  toQuery k v = [(k, coerce v)]

newtype Language = Language (Maybe Text)
  deriving (Show)

instance ToQuery Language where
  toQuery k (Language (Just v)) = [(k, v)]
  toQuery _ (Language Nothing) = []

webAPI :: Builder
webAPI = basicBuilder
  "Dota 2 WebAPI"
  "http://api.steampowered.com"

runWebAPI :: MonadIO m => WebAPIKey -> Language -> WebAPIT m a -> m (Either (APIError ()) a)
runWebAPI k l (WebAPIT act) =
  execAPI webAPI (k, l) $ do
    customizeRoute $ \(Route x ps m) ->
      Route x ( "format" =. ("json" :: Text)
              : "key" =. k
              : "language" =. l
              : ps) m
    act

getLiveLeagueGames :: MonadIO m => WebAPIT m LiveMatchListing
getLiveLeagueGames = fmap getResult $
  WebAPIT $ runRoute $
    Route ["IDOTA2Match_570", "GetLiveLeagueGames", "v1"]
          []
          "GET"

getLeagueListing :: MonadIO m => WebAPIT m LeagueListing
getLeagueListing = fmap getResult $
  WebAPIT $ runRoute $
    Route ["IDOTA2Match_570", "GetLeagueListing", "v1"]
          []
          "GET"

getLeagueMap :: MonadIO m => WebAPIT m (Map LeagueID League)
getLeagueMap = do
  ls <- view leagues <$> getLeagueListing
  return $ Map.fromList $ map (\x -> (view identifier x, x)) ls

getGameItems :: MonadIO m => WebAPIT m ItemListing
getGameItems = fmap getResult $ WebAPIT $ runRoute $
  Route ["IEconDOTA2_570", "GetGameItems", "v1"]
        []
        "GET"

getItemMap :: MonadIO m => WebAPIT m (Map ItemID Item)
getItemMap = do
  is <- view items <$> getGameItems
  return $ Map.fromList $ map (\x -> (view identifier x, x)) is

getPrizePool :: MonadIO m => LeagueID -> WebAPIT m PrizePool
getPrizePool lid = fmap getResult $ WebAPIT $ runRoute $
  Route ["IEconDOTA2_570", "GetTournamentPrizePool", "v1"]
        ["leagueid" =. lid]
        "GET"

getMatchDetails :: MonadIO m => MatchID -> WebAPIT m Match
getMatchDetails m = fmap getResult $ WebAPIT $ runRoute $
  Route ["IDOTA2Match_570", "GetMatchDetails", "v1" ]
        ["match_id" =. m]
        "GET"

getHeroes :: MonadIO m => WebAPIT m HeroListing
getHeroes = fmap getResult $ WebAPIT $ runRoute $
  Route ["IEconDOTA2_570", "GetHeroes", "v1" ]
        []
        "GET"

getHeroMap :: MonadIO m => WebAPIT m (Map HeroID Hero)
getHeroMap = do
  HeroListing hs <- getHeroes
  return $ Map.fromList $ map (\x -> (view identifier x, x)) hs

getPlayerSummaries :: MonadIO m => [AccountID] -> WebAPIT m AccountListing
getPlayerSummaries as = fmap getResult $ WebAPIT $ runRoute $
  Route ["ISteamUser", "GetPlayerSummaries", "v2" ]
        ["steamids" =. map (view as64) as]
        "GET"

getPlayerSummaryMap :: MonadIO m => [AccountID] -> WebAPIT m (Map AccountID64 Account)
getPlayerSummaryMap as = do
  AccountListing accs <- getPlayerSummaries as
  return $ Map.fromList $ map (\x -> (view identifier x, x)) accs
