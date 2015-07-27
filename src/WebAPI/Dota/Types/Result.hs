module WebAPI.Dota.Types.Result where

import Control.Applicative
import Data.Aeson hiding (Result)
import Data.Foldable
import Network.API.Builder
import Prelude

newtype Result a = Result { getResult :: a }
  deriving (Show)

instance FromJSON a => FromJSON (Result a) where
  parseJSON (Object o) = asum
    [ Result <$> o .: "result"
    , Result <$> o .: "response" ]
  parseJSON _ = fail "Result parse failed"

instance FromJSON a => Receivable (Result a) where
  receive = useFromJSON
