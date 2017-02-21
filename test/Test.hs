



import Test.Hspec

import Azubi.Syntax
import Azubi.Model

main :: IO ()
main = hspec $ do
  describe "requires" $ do
    it "should be the same as submoulde for 2 arguments" $ do
       (stateA `requires` stateB) `shouldBe` (submodule [stateB, stateA])



stateA :: State
stateA = State [] [Run "echo" ["you"] Nothing] Nothing
stateB :: State
stateB = State [] [Run "echo" ["me"] Nothing] Nothing
