

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
      (azubi TestContext $ []
       & testFunction) `shouldBe`  [ ShellCommand "got TestContext"]

  describe "CommandContainer (!) CommandContext " $ do
    it "reverts the context" $ do
      (azubi TestContext $ []
       ! testFunction) `shouldBe`  [ ShellCommand "got TestContextReverted"]

  describe "submodule" $ do
    it "should call all commands &" $ do
      (azubi TestContext $ []
        & (submodule $ []
            & testFunction
            ! testFunction)) `shouldBe` [ ShellCommand "got TestContext"
                                        , ShellCommand "got TestContextReverted"]
    it "should call the revert commands for !" $ do
      (azubi TestContext $ []
        ! (submodule $ []
            & testFunction
            ! testFunction)) `shouldBe` [ ShellCommand "got TestContextReverted"
                                        , ShellCommand "got TestContext"]

    it "&? should get called for &" $ do
      (azubi TestContext $ []
        & (submodule $ []
            &? testFunction)) `shouldBe` [ ShellCommand "got TestContext"]

    it "&? should not get called nor reverted for !" $ do
      (azubi TestContext $ []
        ! (submodule $ []
            &? testFunction)) `shouldBe` []

    it "!? should get called for &" $ do
      (azubi TestContext $ []
        & (submodule $ []
            !? testFunction)) `shouldBe` [ ShellCommand "got TestContextReverted" ]

    it "!? should not get called nor reverted for !" $ do
      (azubi TestContext $ []
        ! (submodule $ []
            !? testFunction)) `shouldBe` []
  describe "requires" $ do
    it "returns a Dependency Command" $ do
      (testFunction `requires` testFunction ) TestContext `shouldBe`
        [Dependency [ShellCommand "got TestContext"] [ShellCommand "got TestContext"]]
