


module Commands.TestGit (testGit) where


import Azubi.Commands.Git
import Azubi.Commands.Existance

import Azubi.Core.Command

import Test.Hspec

import TestContext


testGit :: SpecWith ()
testGit = do
  describe "exists (Git \"github.com/me/dotVim\" \"~/.dotVim\")" $ do
    it "returns tests and pull-requests commands" $ do
      (exists (Git "github.com/me/dotVim" "~/.dotVim" []) TestContext) `shouldBe` [
        Dependency {
            body = []
            , dependency = [IfCommand {
                               testCommand = BoolCommand "-d ~/.dotVim"
                               , thenCommand = []
                               , elseCommand = [IfCommand {testCommand = BoolCommand "-d ~"
                                                          , thenCommand = [LogMsg "directory ~ exists"]
                                                          , elseCommand = [InfoMsg "create direcotory ~"
                                                                          ,ShellCommand "mkdir -p ~"]
                                                          }
                                               ,InfoMsg "clone git repository github.com/me/dotVim to folder ~/.dotVim"
                                               ,ShellCommand "git clone github.com/me/dotVim ~/.dotVim"
                                               ]
                               }
                           ]
            }
        ]
    it "returns tests and pull-requests commands for Recursive option" $ do
      (exists (Git "github.com/me/dotVim" "~/.dotVim" [Recursive]) TestContext) `shouldBe` [
        Dependency {
            body = []
            , dependency = [IfCommand {
                               testCommand = BoolCommand "-d ~/.dotVim"
                               , thenCommand = []
                               , elseCommand = [IfCommand {testCommand = BoolCommand "-d ~"
                                                          , thenCommand = [LogMsg "directory ~ exists"]
                                                          , elseCommand = [InfoMsg "create direcotory ~"
                                                                          ,ShellCommand "mkdir -p ~"]
                                                          }
                                               ,InfoMsg "clone git repository github.com/me/dotVim to folder ~/.dotVim"
                                               ,ShellCommand "git clone --recursive github.com/me/dotVim ~/.dotVim"
                                               ]
                               }
                           ]
            }
        ]
    it "returns tests, pull-requests and checkout commands when branch option is given" $ do
      (exists (Git "github.com/me/dotVim" "~/.dotVim" [Branch "develop"]) TestContext) `shouldBe` [
        Dependency {
            body = [ShellCommand "cd ~/.dotVim"
                   ,ShellCommand "git checkout develop"]
            , dependency = [IfCommand {
                               testCommand = BoolCommand "-d ~/.dotVim"
                               , thenCommand = []
                               , elseCommand = [IfCommand {testCommand = BoolCommand "-d ~"
                                                          , thenCommand = [LogMsg "directory ~ exists"]
                                                          , elseCommand = [InfoMsg "create direcotory ~"
                                                                          ,ShellCommand "mkdir -p ~"]
                                                          }
                                               ,InfoMsg "clone git repository github.com/me/dotVim to folder ~/.dotVim"
                                               ,ShellCommand "git clone github.com/me/dotVim ~/.dotVim"
                                               ]
                               }
                           ]
            }
        ]
    it "returns tests and delete commands for reverting" $ do
      (exists (Git "github.com/me/dotVim" "~/.dotVim" []) TestContextReverted) `shouldBe` [
        IfCommand {
            testCommand = BoolCommand "-d ~/.dotVim"
            , thenCommand = [ ShellCommand "rm -fr ~/.dotVim" ]
            , elseCommand = [ LogMsg "~/.dotVim does not exists"]
            }
        ]

