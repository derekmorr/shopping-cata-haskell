module Sample where

import           Data.List          (find, group, sort)
import           Data.List.Grouping (splitEvery)
import qualified Data.Map.Strict    as Map

data MultiPrice p = MultiPrice {
    qty    :: Int
  , mprice :: p
} deriving (Eq, Show)

data PriceData s p = PriceData {
    itemPrices  :: Map.Map s p
  , multiPrices :: Map.Map s (MultiPrice p)
} deriving (Eq, Show)
