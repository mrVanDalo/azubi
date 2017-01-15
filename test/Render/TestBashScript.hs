


module Render.TestBashScript(testBashScript) where

import Test.Hspec

import Azubi.Render.BashScript

import Azubi.Core.Provision
import Azubi.Core.Command
import Azubi.Render.Warez



testUserContext = User None

testBashScript :: SpecWith ()
testBashScript = do
  describe "For IfCommand" $ do
    it "should create if construct with else branch" $ do
      bashScriptGenerator testUserContext [IfCommand{
                                             testCommand=BoolCommand "test"
                                             , thenCommand=[InfoMsg "then-command"]
                                             , elseCommand=[InfoMsg "else-command"]
                                             }] `shouldBe`
        [ "if [[ test ]]; then"
        ,   "echo 'INFO : then-command' | tee -a ~/.azubi.log"
        , "else"
        ,   "echo 'INFO : else-command' | tee -a ~/.azubi.log"
        , "fi"]
    it "should create if construct without else branch when elseCommand is empty" $ do
      bashScriptGenerator testUserContext [IfCommand{
                                             testCommand=BoolCommand "test"
                                             , thenCommand=[InfoMsg "then-command"]
                                             , elseCommand=[]
                                             }] `shouldBe`
        [ "if [[ test ]]; then"
        ,   "echo 'INFO : then-command' | tee -a ~/.azubi.log"
        , "fi"]
    it "should create if construct with pass command in then when thenCommand is empty" $ do
      bashScriptGenerator testUserContext [IfCommand{
                                             testCommand=BoolCommand "test"
                                             , thenCommand=[]
                                             , elseCommand=[InfoMsg "else-command"]
                                             }] `shouldBe`

        [ "if [[ test ]]; then"
        ,   "echo -n ''"
        , "else"
        ,   "echo 'INFO : else-command' | tee -a ~/.azubi.log"
        , "fi"]


  describe "For Dependency" $do
    it "should create a dependency construct" $ do
      bashScriptGenerator testUserContext [ Dependency{ body=[InfoMsg "body-command"]
                                                      , dependency=[ShellCommand "dependency-command"]
                                                      }
                                          ] `shouldBe`
        [ "dependency0=true"
        , "dependency-command &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `dependency-command`' failed | tee -a ~/.azubi.log"
        ,   "dependency0=false"
        , "fi"
        , "if $dependency0 ; then"
        ,   "echo 'INFO : body-command' | tee -a ~/.azubi.log"
        , "fi"]

    it "should create not create a dependency construct for empty and body" $ do
      bashScriptGenerator testUserContext [ Dependency{ body=[]
                                                      , dependency=[ShellCommand "dependency-command"]
                                                      }
                                          ] `shouldBe`
        [ "dependency-command &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `dependency-command`' failed | tee -a ~/.azubi.log"
        , "fi"]

    it "should not create a dependency construct for empty dependency" $ do
      bashScriptGenerator testUserContext [ Dependency{ body=[InfoMsg "body-command"]
                                                      , dependency=[]
                                                      }
                                          ] `shouldBe`
        [ "echo 'INFO : body-command' | tee -a ~/.azubi.log"]

    it "should create nothing for body and dependency empty" $ do
      bashScriptGenerator testUserContext [ Dependency{ body=[]
                                                      , dependency=[]
                                                      }
                                          ] `shouldBe` []

    it "should handle recursive Dependency tree in dependency" $ do
      bashScriptGenerator testUserContext [ Dependency{ body=[InfoMsg "body"]
                                                      , dependency=[Dependency{ body=[ShellCommand "subbody"]
                                                                              , dependency=[ShellCommand "subdependency"]}]
                                                      }
                                          ] `shouldBe`
        [ "dependency0=true"
        , "dependency1=true"
        , "subdependency &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `subdependency`' failed | tee -a ~/.azubi.log"
        ,   "dependency1=false"
        ,   "dependency0=false"
        , "fi"
        , "if $dependency1 ; then"
        ,   "subbody &>> ~/.azubi.log"
        ,   "if [[ $? -ne 0 ]]; then"
        ,     "echo 'Error : running `subbody`' failed | tee -a ~/.azubi.log"
        ,     "dependency0=false"
        ,   "fi"
        , "fi"
        , "if $dependency0 ; then"
        ,   "echo 'INFO : body' | tee -a ~/.azubi.log"
        , "fi"]




    it "should handle recursive Dependency tree in body" $ do
      bashScriptGenerator testUserContext [ Dependency{ body=[Dependency {body=[InfoMsg "body"]
                                                                         , dependency=[ShellCommand "subdependency"]}]
                                                      , dependency=[ShellCommand "dependency"]
                                                      }
                                          ] `shouldBe`
        [ "dependency0=true"
        , "dependency &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `dependency`' failed | tee -a ~/.azubi.log"
        ,   "dependency0=false"
        , "fi"
        , "if $dependency0 ; then"
        ,   "dependency0=true"
        ,   "subdependency &>> ~/.azubi.log"
        ,   "if [[ $? -ne 0 ]]; then"
        ,     "echo 'Error : running `subdependency`' failed | tee -a ~/.azubi.log"
        ,     "dependency0=false"
        ,   "fi"
        ,   "if $dependency0 ; then"
        ,     "echo 'INFO : body' | tee -a ~/.azubi.log"
        ,   "fi"
        , "fi"]

-- | todo : test logging output of the commands to a log-file

  describe "ShellCommand" $ do
    it "should log it's output to the log and check the results" $ do
      bashScriptGenerator testUserContext [ShellCommand "shell-command()"] `shouldBe`
        [ "shell-command() &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `shell-command()`' failed | tee -a ~/.azubi.log"
        , "fi"]
  describe "InfoMsg" $ do
    it "should log the message and echo it" $ do
      bashScriptGenerator testUserContext [InfoMsg "infomsg"] `shouldBe`
        [ "echo 'INFO : infomsg' | tee -a ~/.azubi.log"]

  describe "ErrorMsg" $ do
    it "should log the message and echo it" $ do
      bashScriptGenerator testUserContext [ErrorMsg "errormsg"] `shouldBe`
        [ "echo 'ERROR: errormsg' | tee -a ~/.azubi.log"]

  describe "LogMsg" $ do
    it "should log the message and echo it" $ do
      bashScriptGenerator testUserContext [LogMsg "logmsg"] `shouldBe`
        [ "echo 'INFO : logmsg' &>> ~/.azubi.log"]

  describe "FileContent" $ do
    it "should just write content to file" $ do
      bashScriptGenerator testUserContext [ FileContent "/home/" ["content"] ] `shouldBe`
        ["cat >/home/ <<EOF"
        ,"content"
        ,"EOF"]


  describe "SuperUserSchellCommand" $ do
    it "should echo info for No Root context" $ do
      bashScriptGenerator (User None)[SuperUserShellCommand "super-user-command()"] `shouldBe`
        ["echo 'INFO : cant run superuser command `super-user-command()` as Normal user' | tee -a ~/.azubi.log"]
    it "should echo command and ask for superuser password for User Su context" $ do
      bashScriptGenerator (User Su)[SuperUserShellCommand "super-user-command()"] `shouldBe`
        [ "echo 'INFO : su - -c \"super-user-command()\"' | tee -a ~/.azubi.log"
        , "su - -c \"super-user-command()\" &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `su - -c \"super-user-command()\"`' failed | tee -a ~/.azubi.log"
        , "fi"]

    it "should echo command and ask for user password for User Sudo context" $ do
      bashScriptGenerator (User Sudo)[SuperUserShellCommand "super-user-command()"] `shouldBe`
        [ "echo 'INFO : sudo \"super-user-command()\"' | tee -a ~/.azubi.log"
        ,  "sudo \"super-user-command()\" &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `sudo \"super-user-command()\"`' failed | tee -a ~/.azubi.log"
        , "fi"]

    it "should run and log command for Root context" $ do
      bashScriptGenerator Root [SuperUserShellCommand "super-user-command()"] `shouldBe` 
        [ "if [[ `whoami` != root ]]; then"
        , "  echo 'you must be root to run this script !'"
        , "  exit 1"
        , "fi"
        , ""
        , "super-user-command() &>> ~/.azubi.log"
        , "if [[ $? -ne 0 ]]; then"
        ,   "echo 'Error : running `super-user-command()`' failed | tee -a ~/.azubi.log"
        , "fi"]

-- | todo : escape shellcommand when logging to the file the command
-- |        or add log messages somewhere. and just escape these
-- |        also escape FileContent.




