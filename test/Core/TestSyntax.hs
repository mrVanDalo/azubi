

module Core.TestSyntax (testSyntax) where


import Test.Hspec

import Azubi.Core.Command
import Azubi.Core.Syntax

import TestContext



testFunction :: TestContext -> [ Command ]
testFunction c = [ ShellCommand $ "got " ++ (show c) ]

testFunctionA :: TestContext -> [ Command ]
testFunctionA c = [ ShellCommand $ "funA " ++ (show c) ]

testFunctionB :: TestContext -> [ Command ]
testFunctionB c = [ ShellCommand $ "funB " ++ (show c) ]

testSyntax :: SpecWith ()
testSyntax =  do
  describe "CommandContainer (&) CommandContext " $ do
    it "calls function with Context" $ do
      (azubiConfig TestContext $ []
       & testFunction) `shouldBe`  [ ShellCommand "got TestContext"]

  describe "CommandContainer (!) CommandContext " $ do
    it "reverts the context" $ do
      (azubiConfig TestContext $ []
       ! testFunction) `shouldBe`  [ ShellCommand "got TestContextReverted"]

  describe "submodule" $ do
    it "should call all commands &" $ do
      (azubiConfig TestContext $ []
        & (submodule $ []
            & testFunction
            ! testFunction)) `shouldBe` [ ShellCommand "got TestContext"
                                        , ShellCommand "got TestContextReverted"]
    it "should call the revert commands for !" $ do
      (azubiConfig TestContext $ []
        ! (submodule $ []
            & testFunction
            ! testFunction)) `shouldBe` [ ShellCommand "got TestContextReverted"
                                        , ShellCommand "got TestContext"]

    it "&?& should get called for &" $ do
      (azubiConfig TestContext $ []
        & (submodule $ []
            &?& testFunction)) `shouldBe` [ ShellCommand "got TestContext"]

    it "&?& should not get called nor reverted for !" $ do
      (azubiConfig TestContext $ []
        ! (submodule $ []
            &?& testFunction)) `shouldBe` []

    it "!?& should get called for &" $ do
      (azubiConfig TestContext $ []
        & (submodule $ []
            !?& testFunction)) `shouldBe` [ ShellCommand "got TestContextReverted" ]

    it "!?& should not get called nor reverted for !" $ do
      (azubiConfig TestContext $ []
        ! (submodule $ []
            !?& testFunction)) `shouldBe` []

    it "&?! should get called but not reverted for !" $ do
      (azubiConfig TestContext $ []
        ! (submodule $ []
            &?! testFunction)) `shouldBe` [ ShellCommand "got TestContext"]

    it "&?! should not get called nor reverted for &" $ do
      (azubiConfig TestContext $ []
        & (submodule $ []
            &?! testFunction)) `shouldBe` []

    it "!?! should get called and reverted for !" $ do
      (azubiConfig TestContext $ []
        ! (submodule $ []
            !?! testFunction)) `shouldBe` [ ShellCommand "got TestContextReverted" ]

    it "!?! should not get called nor reverted for &" $ do
      (azubiConfig TestContext $ []
        & (submodule $ []
            !?! testFunction)) `shouldBe` []

  describe "requires" $ do
    it "returns a Dependency Command" $ do
      (testFunctionA `requires` testFunctionB ) TestContext `shouldBe`
        [Dependency [ShellCommand "funA TestContext"] [ShellCommand "funB TestContext"]]

    it "returns the reverted Dependency Command in reverted Context" $ do
      (testFunctionA `requires` testFunctionB ) TestContextReverted `shouldBe`
        [Dependency [ShellCommand "funB TestContextReverted"] [ShellCommand "funA TestContextReverted"]]
