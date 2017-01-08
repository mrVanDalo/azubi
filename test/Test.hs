

import Test.Hspec

import Commands.TestInstall


-- | tests

main :: IO ()
main = hspec $ do
  testInstall
  dummy



dummy :: SpecWith ()
dummy =  do
  describe "dummy test" $ do
    it "will be gone soon" $ do
      head [23 ..] `shouldBe` (23 :: Int)
