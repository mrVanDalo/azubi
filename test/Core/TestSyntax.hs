

module Core.TestSyntax (testSyntax) where


import Test.Hspec

import Core.Command
import Core.Syntax
import TestContext



testFunction :: TestContext -> [ Command ]
testFunction c = [ ShellCommand $ "got " ++ (show c) ]


testSyntax :: SpecWith ()
testSyntax =  do
  describe "CommandContainer (&) CommandContext " $ do
    it "calls function with Context" $ do
      (azubi TestContext & testFunction) `shouldBe`  (CommandContainer TestContext [ ShellCommand "got TestContext"])
  describe "CommandContainer (!) CommandContext " $ do
    it "reverts the context" $ do
      (azubi TestContext ! testFunction) `shouldBe`  (CommandContainer TestContext [ ShellCommand "got TestContextReverted"])
