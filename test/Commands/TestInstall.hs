

module Commands.TestInstall(testInstall) where


import Test.Hspec
import Core.Command
import Core.Context
import Commands.Install
import Core.Revertable


-- | test-setup

data TestInstall = TestInstall
                 | TestInstallReverted

instance Context TestInstall where
  label _ = "test-context"

instance Revertable TestInstall where
  isRevert TestInstall = False
  isRevert TestInstallReverted = True

  toggleRevert TestInstall = TestInstallReverted
  toggleRevert TestInstallReverted = TestInstall


instance Installed TestInstall where
  isInstalled _ pkg = BoolCommand $ "check " ++ pkg 
  doInstall _ pkg = [ ShellCommand $ "install " ++ pkg ]
  doUnInstall _ pkg = [ ShellCommand $ "uninstall " ++ pkg ]



-- | the tests

testInstall :: SpecWith ()
testInstall =  do
  describe "installed \"vim\"" $ do
    it "returns Install Commands" $ do
      (installed "vim" TestInstall) `shouldBe` [
        IfCommand {
            testCommand = BoolCommand "check vim",
              thenCommand = [InfoMsg "vim is already installed"],
              elseCommand = [InfoMsg "installing vim",ShellCommand "install vim"]}]
    context "when in Reverted Context" $ do
      it "returns Uninstall Commands " $ do
        (installed "vim" TestInstallReverted) `shouldBe` [
          IfCommand {
              testCommand = BoolCommand "check vim",
                thenCommand = [InfoMsg "uninstalling vim",ShellCommand "uninstall vim"],
                elseCommand = [InfoMsg "vim is not installed"]}]
