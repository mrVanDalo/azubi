

import Test.Hspec

import Core.TestSyntax

import Commands.TestInstall
import Commands.TestFile
import Commands.TestGit

import Render.TestBashScript

-- | tests

main :: IO ()
main = hspec $ do
  testInstall
  testFile
  testSyntax
  testGit
  testBashScript

