I worked through the [shopping cart kata](http://codekata.com/kata/kata09-back-to-the-checkout/) last weekend. I had a few ideas for how to approach it. I did the examples in Haskell.

`PriceData` is just a basic record with [Map.Strict](https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Map-Strict.html):

```haskell
import qualified Data.Map.Strict    as Map

data MultiPrice = MultiPrice {
    qty    :: Int
  , mprice :: Int
} deriving (Eq, Show)

data PriceData = PriceData {
    itemPrices  :: Map.Map Char Int
  , multiPrices :: Map.Map Char MultiPrice
} deriving (Eq, Show)
```

I defined some sample price data (from the tests):

```haskell
priceData :: PriceData
priceData = PriceData {
    itemPrices = Map.fromList [ ('A', 50), ('B', 30), ('C', 20), ('D', 15) ]
  , multiPrices = Map.fromList [
      ('A', MultiPrice { qty = 3, mprice = 130 })
    , ('B', MultiPrice { qty = 2, mprice = 45 })
  ]
}
```



# Implementation #1

I focused on the `checkout` function, which transforms a `String` into an `Int`:

```haskell
checkout :: PriceData -> String -> Int
```

I had the idea to transform the `String`, the basket, into a list of `String`s. The size of each `String` is determined by the price data -- if there's a multiprice defined, the string is grouped by the qty:

```haskell
import Data.List (group, sort)
mkItemLists :: String -> [String]
mkItemLists = group . sort
```

Now we can load it into `ghci`:

```haskell
λ> let basket = "AAABBCACB"
λ> let groupedItems = mkItemLists basket
λ> groupedItems 
["AAAA","BBB","CC"]
```

Now we transform each `String` into a `[String]` if there's a multiPrice defined:

```haskell
-- splits grouped strings into multiprice-sized chunks if there's a multiprice
splitItems :: PriceData -> String -> [String]
splitItems pd str = case Map.lookup (head str) (multiPrices pd) of
    Just (MultiPrice q _) -> splitEvery q str
    Nothing               -> [str]
```

In `ghci`:

```haskell
λ> let chunkedItems = concatMap (splitItems priceData) groupedItems
λ> chunkedItems 
["AAA","A","BB","B","CC"]
```

Now we make a function that computes the price from each chunk. If there's a multlPrice defined for the item and the length == the multiprice quantity, we use it. Otherwise, we take the individual item price * the count:

```haskell
chunkPrice :: Int -> Maybe MultiPrice -> String -> Int
chunkPrice ip Nothing s = length s * ip
chunkPrice ip (Just mp) s
    | length s == qty mp = mprice mp
    | otherwise          = length s * ip
```

Now we just `fmap` the `chunkPrice` function over our list of `Strings` (plus some wrapper code):

```haskell
checkout :: PriceData -> String -> Int
checkout pd basket = sum partialTotals
  where groupedItems = mkItemLists basket
        chunkedItems = concatMap (splitItems pd) groupedItems
        partialTotals = fmap (cp pd) chunkedItems

cp :: PriceData -> String -> Int
cp pd str = chunkPrice ip mp str
    where ip = itemPrices pd Map.! head str
          mp = Map.lookup (head str) (multiPrices pd)
```

This works, but I decided it was too much list processing. There are a lot of private helper functions. It just seemed like too much code (about 40 lines).

# Implementation 2

I decided it would be easier to traverse the basket list once to build up a count of items -> counts. Then use the counts to build the prices.

Step 1: Build a `Map Char Int`:

```haskell
countPerItem :: String -> Map.Map Char Int
countPerItem basket = Map.fromListWith (+) [(c, 1) | c <- basket]
```

This works in `ghci`:

```haskell
λ> basket 
"AAABBCACB"
λ> countPerItem basket 
fromList [('A',4),('B',3),('C',2)]
```

We can use `div` and `rem` to compute prices:

```haskell
subTotal :: Int -> Int -> Maybe MultiPrice -> Int
subTotal count iprice Nothing   = iprice * count
subTotal count iprice (Just mp) = count `div` q * mprice mp + count `rem` q * iprice
    where q = qty mp
```

Now we fmap each entry in the Map to an Int, then reduce with + :

```haskell
-- wrapper around subtTotal that handles the Map operations
totalPerItem :: PriceData -> (Char, Int) -> Int
totalPerItem pd (sku, count) = subTotal count iprice mp
    where iprice = itemPrices pd Map.! sku
          mp     = Map.lookup sku (multiPrices pd)

checkout' :: PriceData -> String -> Int
checkout' pd basket = sum subTotals
    where itemCounts = Map.toList $ countPerItem basket
          subTotals  = fmap (totalPerItem pd) itemCounts
```

This seems better. Much less code (about 30 lines).



# Implementation 2A

In implementation 2, we take a `String`,  convert it to a `[Int]`, and reduce. There's an intermediate list that we don't need. We can use `foldr` to avoid building up intermediate state:

```haskell
-- same as checkout' but avoid building an intermediate map
checkout'' :: PriceData -> String -> Int
checkout'' pd basket = foldr f 0 itemCounts
    where itemCounts = Map.toList $ countPerItem basket
          f skuCount total = total + totalPerItem pd skuCount
```

