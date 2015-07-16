module WebAPI.Dota.Types.Result where

import Data.Aeson hiding (Result)
import Network.API.Builder

newtype Result a = Result { getResult :: a }
  deriving (Show)

instance FromJSON a => FromJSON (Result a) where
  parseJSON (Object o) = Result <$> o .: "result"
  parseJSON _ = fail "Result parse failed"

instance FromJSON a => Receivable (Result a) where
  receive = useFromJSON
