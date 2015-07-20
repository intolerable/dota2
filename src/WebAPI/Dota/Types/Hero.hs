module WebAPI.Dota.Types.Hero where

--import WebAPI.Dota.Internal.SharedFields

import Control.Applicative
import Data.Aeson
import Data.Foldable
import Prelude

newtype HeroID = HeroID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON HeroID where
  parseJSON (Object o) = HeroID <$> o .: "hero_id"
  parseJSON x = asum
    [ HeroID <$> parseJSON x
    , fail "HeroID parse failed" ]
