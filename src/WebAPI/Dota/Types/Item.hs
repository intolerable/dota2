module WebAPI.Dota.Types.Item
  ( ItemBuild(..)
  , ItemListing(ItemListing)
  , Item(Item)
  , ItemID(..)
  , SF.HasIdentifier(..)
  , SF.HasName(..)
  , SF.HasLocalName(..)
  , SF.HasCost(..)
  , SF.HasSecretShop(..)
  , SF.HasSideShop(..)
  , SF.HasRecipe(..)
  , SF.HasItems(..) ) where

import WebAPI.Dota.Internal.SharedFields as SF

import Control.Applicative
import Control.Lens.TH
import Data.Aeson
import Data.Foldable
import Data.Text (Text)
import Network.API.Builder
import Prelude

data ItemBuild =
  ItemBuild ItemID ItemID ItemID ItemID ItemID ItemID
  deriving (Show, Read, Eq)

newtype ItemListing = ItemListing { _itemListingItems :: [Item] }
  deriving (Show, Eq)

instance FromJSON ItemListing where
  parseJSON (Object o) = ItemListing <$> o .: "items"
  parseJSON _ = fail "ItemListing parse failed"

instance Receivable ItemListing where receive = useFromJSON

data Item =
  Item { _itemIdentifier :: ItemID
       , _itemName :: Text
       , _itemLocalName :: Text
       , _itemCost :: Integer
       , _itemSecretShop :: Bool
       , _itemSideShop :: Bool
       , _itemRecipe :: Bool }
  deriving (Show, Eq)

instance FromJSON Item where
  parseJSON (Object o) =
    Item <$> o .: "id"
         <*> o .: "name"
         <*> o .: "localized_name"
         <*> o .: "cost"
         <*> (toEnum <$> o .: "secret_shop")
         <*> (toEnum <$> o .: "side_shop")
         <*> (toEnum <$> o .: "recipe")
  parseJSON _ = fail "Item parse failed"

newtype ItemID = ItemID Integer
  deriving (Show, Read, Eq, Ord)

instance FromJSON ItemID where
  parseJSON (Object o) = ItemID <$> o .: "item_id"
  parseJSON x = asum
    [ ItemID <$> parseJSON x
    , fail "ItemID parse failed" ]

makeFields ''Item
makeFields ''ItemListing
