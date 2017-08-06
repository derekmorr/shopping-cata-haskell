import qualified Data.Map.Strict as Map
import           Lib
import           Test.Hspec

priceData :: PriceData Char
priceData = PriceData {
    itemPrices = Map.fromList [ ('A', 50), ('B', 30), ('C', 20), ('D', 15) ]
  , multiPrices = Map.fromList [
      ('A', MultiPrice { qty = 3, mprice = 130 })
    , ('B', MultiPrice { qty = 2, mprice = 45 })
  ]
}

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

totalsSpec :: (PriceData Char -> String -> Int) -> (String, Int) -> Spec
totalsSpec f (item, expectedPrice) =
  it item $
    f priceData item `shouldBe` expectedPrice

main :: IO ()
main = hspec $
  describe "blah" $ do
    describe "shopping cart - checkout" $ do
      mapM_ (totalsSpec checkout) totalsExamples
    describe "shoping cart - checkout'" $ do
      mapM_ (totalsSpec checkout') totalsExamples
    describe "shoping cart - checkout''" $ do
      mapM_ (totalsSpec checkout'') totalsExamples
    describe "shoping cart - checkout'''" $ do
      mapM_ (totalsSpec checkout''') totalsExamples
