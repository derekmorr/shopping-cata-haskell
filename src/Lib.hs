module Lib where

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

checkout :: (Ord s, Num p) => PriceData s p -> [s] -> p
checkout pd basket = sum partialTotals
  where groupedItems = mkItemLists basket
        chunkedItems = concatMap (splitItems pd) groupedItems
        partialTotals = fmap (cp pd) chunkedItems

mkItemLists :: Ord s => [s] -> [[s]]
mkItemLists = group . sort

cp :: (Ord s, Num p) => PriceData s p -> [s] -> p
cp pd str = chunkPrice ip mp str
    where ip = itemPrices pd Map.! head str
          mp = Map.lookup (head str) (multiPrices pd)

chunkPrice :: Num p => p -> Maybe (MultiPrice p) -> [s] -> p
chunkPrice ip Nothing s = ip * fromIntegral (length s)
chunkPrice ip (Just mp) s
    | length s == qty mp = mprice mp
    | otherwise          = ip * fromIntegral (length s)

-- splits grouped strings into multiprice-sized chunks if there's a multiprice
splitItems :: (Ord s, Num p) => PriceData s p -> [s] -> [[s]]
splitItems pd str = case Map.lookup (head str) (multiPrices pd) of
    Just (MultiPrice q _) -> splitEvery q str
    Nothing               -> [str]

-- second implementation

countPerItem :: Ord s => [s] -> Map.Map s Int
countPerItem basket = Map.fromListWith (+) [(c, 1) | c <- basket]

subTotal :: Num p => Int -> p -> Maybe (MultiPrice p) -> p
subTotal count iprice Nothing   = iprice * fromIntegral count
subTotal count iprice (Just mp) = mpCount * mprice mp + ipCount * iprice
    where q = qty mp
          mpCount = fromIntegral (count `div` q)
          ipCount = fromIntegral (count `rem` q)

totalPerItem :: (Ord s, Num p) => PriceData s p -> (s, Int) -> p
totalPerItem pd (sku, count) = subTotal count iprice mp
    where iprice = itemPrices pd Map.! sku
          mp     = Map.lookup sku (multiPrices pd)

checkout' :: (Ord s, Num p) => PriceData s p -> [s] -> p
checkout' pd basket = sum subTotals
    where itemCounts = Map.toList $ countPerItem basket
          subTotals  = fmap (totalPerItem pd) itemCounts

-- same as checkout' but avoid building an intermediate map
checkout'' :: (Ord s, Num p) => PriceData s p -> [s] -> p
checkout'' pd basket = foldr f 0 itemCounts
    where itemCounts = Map.toList $ countPerItem basket
          f skuCount total = total + totalPerItem pd skuCount

-- same as checkout'' but point free
checkout''' :: (Ord s, Num p) => PriceData s p -> [s] -> p
checkout''' pd basket = foldr f 0 itemCounts
    where itemCounts = Map.toList $ countPerItem basket
          f = (+) . totalPerItem pd
