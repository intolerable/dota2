module WebAPI.Dota.Types.Hero
  ( HeroID(..)
  , HeroListing(HeroListing)
  , Hero(Hero)
  , SF.HasHeroes(..)
  , SF.HasIdentifier(..)
  , SF.HasLocalName(..)
  , SF.HasName(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Foldable
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Receive
import Prelude

newtype HeroID = HeroID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON HeroID where
  parseJSON (Object o) = HeroID <$> o .: "hero_id"
  parseJSON x = asum
    [ HeroID <$> parseJSON x
    , fail "HeroID parse failed" ]

newtype HeroListing = HeroListing { _heroListingHeroes :: [Hero] }
  deriving (Show, Read, Eq)

instance FromJSON HeroListing where
  parseJSON (Object o) = HeroListing <$> o .: "heroes"
  parseJSON _ = mempty

instance Receivable HeroListing where receive = useFromJSON

data Hero =
  Hero { _heroIdentifier :: HeroID
       , _heroName :: Text
       , _heroLocalName :: Text }
  deriving (Show, Read, Eq)

instance FromJSON Hero where
  parseJSON (Object o) =
    Hero <$> o .: "id"
         <*> o .: "name"
         <*> o .: "localized_name"
  parseJSON _ = mempty

makeFields ''Hero
makeFields ''HeroListing
