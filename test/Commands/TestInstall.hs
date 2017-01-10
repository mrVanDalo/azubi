

module Commands.TestInstall(testInstall) where


import Test.Hspec
import Core.Command
import Commands.Install

import TestContext




-- | the tests

testInstall :: SpecWith ()
testInstall =  do
  describe "installed \"vim\"" $ do
    it "returns Install Commands" $ do
      (installed "vim" TestContext) `shouldBe` [
        IfCommand {
            testCommand = BoolCommand "check vim",
              thenCommand = [InfoMsg "vim is already installed"],
              elseCommand = [InfoMsg "installing vim", SuperUserShellCommand "install vim"]}]
    context "when in Reverted Context" $ do
      it "returns Uninstall Commands " $ do
        (installed "vim" TestContextReverted) `shouldBe` [
          IfCommand {
              testCommand = BoolCommand "check vim",
                thenCommand = [InfoMsg "uninstalling vim", SuperUserShellCommand "uninstall vim"],
                elseCommand = [InfoMsg "vim is not installed"]}]
