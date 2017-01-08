

import Test.Hspec

import Commands.TestInstall
import Commands.TestFile
import Core.TestSyntax 
-- | tests

main :: IO ()
main = hspec $ do
  testInstall
  testFile
  testSyntax

