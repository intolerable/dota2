module WebAPI.Dota.Types.Player
  ( PlayerAccount(PlayerAccount)
  , Player(Player)
  , SF.HasSlot(..)
  , SF.HasAccountID(..)
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
  , SF.HasRespawnTimer(..)
  , SF.HasPosition(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF
import WebAPI.Dota.Types.Hero
import WebAPI.Dota.Types.Item

import Control.Lens.TH
import Data.Text (Text)
import Data.Aeson

data PlayerAccount =
  PlayerAccount { _playerAccountIdentifier :: Integer
                , _playerAccountName :: Text
                , _playerAccountHeroID :: Integer
                , _playerAccountTeam :: Integer }
  deriving (Show, Read, Eq, Ord)

instance FromJSON PlayerAccount where
  parseJSON (Object o) =
    PlayerAccount <$> o .: "account_id"
                  <*> o .: "name"
                  <*> o .: "hero_id"
                  <*> o .: "team"
  parseJSON _ = fail "PlayerAccount parse failed"

data Player =
  Player { _playerSlot :: Integer
         , _playerAccountID :: Integer
         , _playerHeroID :: HeroID
         , _playerKills :: Maybe Integer
         , _playerDeaths :: Maybe Integer
         , _playerAssists :: Maybe Integer
         , _playerLastHits :: Maybe Integer
         , _playerDenies :: Maybe Integer
         , _playerGold :: Maybe Integer
         , _playerLevel :: Maybe Integer
         , _playerGpm :: Maybe Integer
         , _playerXpm :: Maybe Integer
         , _playerItems :: Maybe ItemBuild
         , _playerRespawnTimer :: Maybe Integer
         , _playerPosition :: (Double, Double) }
  deriving (Show, Eq)

instance FromJSON Player where
  parseJSON (Object o) =
    Player <$> o .: "player_slot"
           <*> o .: "account_id"
           <*> o .: "hero_id"
           <*> o .:? "kills"
           <*> o .:? "deaths"
           <*> o .:? "assists"
           <*> o .:? "last_hits"
           <*> o .:? "denies"
           <*> o .:? "gold"
           <*> o .:? "level"
           <*> o .:? "gold_per_min"
           <*> o .:? "xp_per_min"
           <*> (liftA6 ItemBuild <$> o .:? "item0"
                                 <*> o .:? "item1"
                                 <*> o .:? "item2"
                                 <*> o .:? "item3"
                                 <*> o .:? "item4"
                                 <*> o .:? "item5")
           <*> o .:? "respawn_timer"
           <*> ((,) <$> o .: "position_x" <*> o .: "position_x")
  parseJSON _ = fail "Player parse failed"

liftA6 :: Applicative x
       => (a -> b -> c -> d -> e -> f -> g)
       -> x a
       -> x b
       -> x c
       -> x d
       -> x e
       -> x f
       -> x g
liftA6 x a b c d e f =
  x <$> a <*> b <*> c <*> d <*> e <*> f

makeFields ''Player
makeFields ''PlayerAccount
