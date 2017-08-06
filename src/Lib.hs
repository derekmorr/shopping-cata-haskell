module Lib where

import           Data.List          (find, group, sort)
import           Data.List.Grouping (splitEvery)
import qualified Data.Map.Strict    as Map

data MultiPrice = MultiPrice {
    qty    :: Int
  , mprice :: Int
} deriving (Eq, Show)

data PriceData s = PriceData {
    itemPrices  :: Map.Map s Int
  , multiPrices :: Map.Map s MultiPrice
} deriving (Eq, Show)

checkout :: Ord s => PriceData s -> [s] -> Int
checkout pd basket = sum partialTotals
  where groupedItems = mkItemLists basket
        chunkedItems = concatMap (splitItems pd) groupedItems
        partialTotals = fmap (cp pd) chunkedItems

mkItemLists :: Ord s => [s] -> [[s]]
mkItemLists = group . sort

cp :: Ord s => PriceData s -> [s] -> Int
cp pd str = chunkPrice ip mp str
    where ip = itemPrices pd Map.! head str
          mp = Map.lookup (head str) (multiPrices pd)

chunkPrice :: Int -> Maybe MultiPrice -> [s] -> Int
chunkPrice ip Nothing s = length s * ip
chunkPrice ip (Just mp) s
    | length s == qty mp = mprice mp
    | otherwise          = length s * ip

-- splits grouped strings into multiprice-sized chunks if there's a multiprice
splitItems :: Ord s => PriceData s -> [s] -> [[s]]
splitItems pd str = case Map.lookup (head str) (multiPrices pd) of
    Just (MultiPrice q _) -> splitEvery q str
    Nothing               -> [str]

-- second implementation

countPerItem :: Ord s => [s] -> Map.Map s Int
countPerItem basket = Map.fromListWith (+) [(c, 1) | c <- basket]

subTotal :: Int -> Int -> Maybe MultiPrice -> Int
subTotal count iprice Nothing   = iprice * count
subTotal count iprice (Just mp) = count `div` q * mprice mp + count `rem` q * iprice
    where q = qty mp

totalPerItem :: Ord s => PriceData s -> (s, Int) -> Int
totalPerItem pd (sku, count) = subTotal count iprice mp
    where iprice = itemPrices pd Map.! sku
          mp     = Map.lookup sku (multiPrices pd)

checkout' :: Ord s => PriceData s -> [s] -> Int
checkout' pd basket = sum subTotals
    where itemCounts = Map.toList $ countPerItem basket
          subTotals  = fmap (totalPerItem pd) itemCounts

-- same as checkout' but avoid building an intermediate map
checkout'' :: Ord s => PriceData s -> [s] -> Int
checkout'' pd basket = foldr f 0 itemCounts
    where itemCounts = Map.toList $ countPerItem basket
          f skuCount total = total + totalPerItem pd skuCount

-- same as checkout'' but point free
checkout''' :: Ord s => PriceData s -> [s] -> Int
checkout''' pd basket = foldr f 0 itemCounts
    where itemCounts = Map.toList $ countPerItem basket
          f = (+) . totalPerItem pd
