

import Test.Hspec

import Core.TestSyntax

import Commands.TestInstall
import Commands.TestFile
import Commands.TestGit

-- | tests

main :: IO ()
main = hspec $ do
  testInstall
  testFile
  testSyntax
  testGit

