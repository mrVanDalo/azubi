

module Commands.TestFile (testFile) where



import Commands.File

import Test.Hspec
import Core.Command

import TestContext





testFile :: SpecWith ()
testFile =  do
  describe "exists (File \"/etc/azubi/conf\")" $ do
    it "returns test and create Commands" $ do
      (exists (File "/etc/azubi/conf") TestContext) `shouldBe` [
        IfCommand {
            testCommand = BoolCommand "-e /etc/azubi/conf",
            thenCommand = [InfoMsg "/etc/azubi/conf already exists"],
            elseCommand = [ IfCommand {
                              testCommand = BoolCommand "-d /etc/azubi",
                              thenCommand = [InfoMsg "directory /etc/azubi exists"],
                              elseCommand = [InfoMsg "create direcotory /etc/azubi"
                                            , ShellCommand "mkdir -p /etc/azubi"]
                              }
                            , InfoMsg "check existence of /etc/azubi/conf",
                              ShellCommand "touch /etc/azubi/conf"
                            ]
            }
        ]
  describe "exists (Symlink \"/etc/conf.d/azubi\" \"/etc/azubi/conf\")" $ do
    it "returns test and create Commands" $ do
      (exists (Symlink "/etc/conf.d/azubi" "/etc/azubi/conf") TestContext) `shouldBe` [
        IfCommand {
            testCommand = BoolCommand "-d /etc/conf.d",
              thenCommand = [InfoMsg "directory /etc/conf.d exists"],
              elseCommand = [InfoMsg "create direcotory /etc/conf.d"
                            , ShellCommand "mkdir -p /etc/conf.d"]
            }
        , InfoMsg "create symlink /etc/conf.d/azubi -> /etc/azubi/conf"
        , ShellCommand "ln -s /etc/azubi/conf /etc/conf.d/azubi"
        ]
  describe "exists (Directory \"/etc/azubi/conf\")" $ do
    it "returns test and create Commands" $ do
      (exists (Directory "/etc/azubi") TestContext) `shouldBe` [
        IfCommand {
            testCommand = BoolCommand "-d /etc/azubi",
              thenCommand = [InfoMsg "directory /etc/azubi exists"],
              elseCommand = [InfoMsg "create direcotory /etc/azubi"   -- | todo : checken ob es ein file gibt, und stattdessen dann löschen
                            , ShellCommand "mkdir -p /etc/azubi"]
            }
        ]
