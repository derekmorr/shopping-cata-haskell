import           Lib        (price)
import           Test.Hspec

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
    price item `shouldBe` expectedPrice

main :: IO ()
main = hspec $
  describe "shopping cart" $ do
    mapM_ totalsSpec totalsExamples

