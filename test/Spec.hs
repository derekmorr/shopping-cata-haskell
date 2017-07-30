import qualified Data.Map.Strict as Map
import           Lib
import           Test.Hspec

-- priceData :: PriceData
-- priceData = PriceData {
--   itemPrices = [
--     ItemPrice { isku = 'A', iprice = 50 }
--   , ItemPrice { isku = 'B', iprice = 20 }
--   , ItemPrice { isku = 'C', iprice = 20 }
--   , ItemPrice { isku = 'D', iprice = 15 }
--   ],
--   multiPrices = [
--     MultiPrice { msku = 'A', qty = 3, mprice = 130 }
--   , MultiPrice { msku = 'B', qty = 2, mprice = 45 }
--   ]
-- }

priceData :: PriceData
priceData = PriceData {
    itemPrices = Map.fromList [ ('A', 50), ('B', 30), ('C', 20), ('D', 15) ]
  , multiPrices = Map.fromList [
      ('A', MultiPrice { qty = 3, mprice = 130 })
    , ('B', MultiPrice { qty = 2, mprice = 45 })
  ]
}

-- priceData :: PriceData
-- priceData = PriceData {
--   itemPrices = [
--     ('A', 50)
--   , ('B', 20)
--   , ('C', 20)
--   , ('D', 15)
--   ],
--   multiPrices = [
--     ('A', 3, 130)
--   , ('B', 2, 45)
--   ]
-- }

totalsExamples :: [(String, Int)]
totalsExamples =
  [ ("", 0)
  , ("A", 50)
  , ("AB", 80)
  , ("CDBA", 115)

  , ("AA", 100)
  , ("AAA", 130)
  , ("AAAA", 180)
  , ("AAAAA", 230)
  , ("AAAAAA", 260)

  , ("AAAB", 160)
  , ("AAABB", 175)
  , ("AAABBD", 190)
  , ("DABABA", 190)
  ]

totalsSpec :: (String, Int) -> Spec
totalsSpec (item, expectedPrice) =
  -- it ("price of " ++ item) $
  it item $
    checkout priceData item `shouldBe` expectedPrice

main :: IO ()
main = hspec $
  describe "shopping cart" $ do
    mapM_ totalsSpec totalsExamples

