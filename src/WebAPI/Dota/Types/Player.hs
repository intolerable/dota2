module WebAPI.Dota.Types.Player
  ( AccountID(..)
  , Account(Account)
  , AccountListing(AccountListing)
  , LivePlayerAccount(LivePlayerAccount)
  , LivePlayer(LivePlayer)
  , Player(Player)
  , kda
  , cs
  , SF.HasSlot(..)
  , SF.HasAccountID(..)
  , SF.HasAccounts(..)
  , SF.HasName(..)
  , SF.HasHeroID(..)
  , SF.HasKills(..)
  , SF.HasDeaths(..)
  , SF.HasAssists(..)
  , SF.HasLastHits(..)
  , SF.HasDenies(..)
  , SF.HasGold(..)
  , SF.HasLevel(..)
  , SF.HasGpm(..)
  , SF.HasXpm(..)
  , SF.HasItems(..)
  , SF.HasTowerDamage(..)
  , SF.HasFaction(..)
  , SF.HasHeroDamage(..)
  , SF.HasHeroHealing(..)
  , SF.HasGoldSpent(..)
  , SF.HasRespawnTimer(..)
  , SF.HasPosition(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF
import WebAPI.Dota.Types.Hero
import WebAPI.Dota.Types.Item
import WebAPI.Dota.Types.Team

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Word
import Data.Bits
import Data.Text (Text)
import Network.API.Builder.Query
import Prelude

newtype AccountID = AccountID Integer
  deriving (Show, Read, Eq, Ord)

instance ToQuery AccountID where
  toQuery k (AccountID v) = toQuery k v

instance FromJSON AccountID where
  parseJSON x = AccountID <$> parseJSON x

newtype AccountListing = AccountListing { _accountListingAccounts :: [Account] }
  deriving (Show, Read, Eq)

instance FromJSON AccountListing where
  parseJSON x = AccountListing <$> parseJSON x

data Account =
  Account { _accountIdentifier :: AccountID
          , _accountName :: Text }
  deriving (Show, Read, Eq)

instance FromJSON Account where
  parseJSON (Object o) =
    Account <$> o .: "steamid"
            <*> o .: "personaname"
  parseJSON _ = fail "Account parse failed"

data LivePlayerAccount =
  LivePlayerAccount { _livePlayerAccountIdentifier :: AccountID
                    , _livePlayerAccountName :: Text
                    , _livePlayerAccountHeroID :: Integer
                    , _livePlayerAccountTeam :: Integer }
  deriving (Show, Read, Eq, Ord)

instance FromJSON LivePlayerAccount where
  parseJSON (Object o) =
    LivePlayerAccount <$> o .: "account_id"
                      <*> o .: "name"
                      <*> o .: "hero_id"
                      <*> o .: "team"
  parseJSON _ = fail "LivePlayerAccount parse failed"

data LivePlayer =
  LivePlayer { _livePlayerSlot :: Integer
             , _livePlayerAccountID :: AccountID
             , _livePlayerHeroID :: HeroID
             , _livePlayerKills :: Integer
             , _livePlayerDeaths :: Integer
             , _livePlayerAssists :: Integer
             , _livePlayerLastHits :: Integer
             , _livePlayerDenies :: Integer
             , _livePlayerGold :: Integer
             , _livePlayerLevel :: Integer
             , _livePlayerGpm :: Integer
             , _livePlayerXpm :: Integer
             , _livePlayerItems :: ItemBuild
             , _livePlayerRespawnTimer :: Integer
             , _livePlayerPosition :: (Double, Double) }
  deriving (Show, Eq)

instance FromJSON LivePlayer where
  parseJSON (Object o) =
    LivePlayer <$> o .: "player_slot"
               <*> o .: "account_id"
               <*> o .: "hero_id"
               <*> o .: "kills"
               <*> o .: "death"
               <*> o .: "assists"
               <*> o .: "last_hits"
               <*> o .: "denies"
               <*> o .: "gold"
               <*> o .: "level"
               <*> o .: "gold_per_min"
               <*> o .: "xp_per_min"
               <*> (ItemBuild <$> o .: "item0"
                              <*> o .: "item1"
                              <*> o .: "item2"
                              <*> o .: "item3"
                              <*> o .: "item4"
                              <*> o .: "item5")
               <*> o .: "respawn_timer"
               <*> ((,) <$> o .: "position_x" <*> o .: "position_x")
  parseJSON _ = fail "LivePlayer parse failed"

data Player =
  Player { _playerAccountID :: AccountID
         , _playerHeroID :: HeroID
         , _playerSlot :: Integer
         , _playerFaction :: Faction
         , _playerKills :: Integer
         , _playerDeaths :: Integer
         , _playerAssists :: Integer
         , _playerLastHits :: Integer
         , _playerDenies :: Integer
         , _playerGold :: Integer
         , _playerLevel :: Integer
         , _playerGpm :: Integer
         , _playerXpm :: Integer
         , _playerGoldSpent :: Integer
         , _playerHeroDamage :: Integer
         , _playerTowerDamage :: Integer
         , _playerHeroHealing :: Integer
         , _playerItems :: ItemBuild }
  deriving (Show, Read, Eq)

instance FromJSON Player where
  parseJSON (Object o) =
    Player <$> o .: "account_id"
           <*> o .: "hero_id"
           <*> o .: "player_slot"
           <*> (extractFaction <$> o .: "player_slot")
           <*> o .: "kills"
           <*> o .: "deaths"
           <*> o .: "assists"
           <*> o .: "last_hits"
           <*> o .: "denies"
           <*> o .: "gold"
           <*> o .: "level"
           <*> o .: "gold_per_min"
           <*> o .: "xp_per_min"
           <*> o .: "gold_spent"
           <*> o .: "hero_damage"
           <*> o .: "tower_damage"
           <*> o .: "hero_healing"
           <*> (ItemBuild <$> o .: "item_0"
                          <*> o .: "item_1"
                          <*> o .: "item_2"
                          <*> o .: "item_3"
                          <*> o .: "item_4"
                          <*> o .: "item_5")
  parseJSON _ = fail "Player parse failed"

extractFaction :: Word8 -> Faction
extractFaction n = if n `testBit` 7 then Dire else Radiant

kda :: (HasKills s a, HasDeaths s a, HasAssists s a) => Getter s (a, a, a)
kda = to $ (,,) <$> view kills <*> view deaths <*> view assists

cs :: (HasLastHits s a, HasDenies s a) => Getter s (a, a)
cs = to $ (,) <$> view lastHits <*> view denies

makeFields ''Account
makeFields ''AccountListing
makeFields ''LivePlayer
makeFields ''Player
makeFields ''LivePlayerAccount
