
import Test.Hspec

import Azubi.Core.StateExecutors.LocalUnixStateExecutor
import Azubi
import System.Directory
import Azubi.Core.StateExecutor

verbosity :: Verbosity
verbosity = Verbose

main :: IO ()
main = hspec $ do

  describe "whatIsBehind' should identify" $ do
    it "a Folder as IsFolder" $ do
      folderType <- whatIsBehind' folderPath
      folderType `shouldBe` IsFolder
    it "a File as IsFile" $ do
      folderType <- whatIsBehind' filePath
      folderType `shouldBe` IsFile
    it "a Link as IsSymlink " $ do
      folderType <- whatIsBehind' linkPath
      folderType `shouldBe` IsSymlink "./file"
    it "a Folder Link as IsSymlink " $ do
      folderType <- whatIsBehind' folderLinkPath
      folderType `shouldBe` IsSymlink "./folder"
    it "~/.cabal as IsFolder" $ do
      folderType <- whatIsBehind' "~/.cabal"
      folderType `shouldBe` IsFolder
    it "/usr as IsFolder" $ do
      folderType <- whatIsBehind' "/usr"
      folderType `shouldBe` IsFolder
    it "/donotexist as DoesNotExist" $ do
      noneType <- whatIsBehind' "/donotexist"
      noneType `shouldBe` DoesNotExist

  describe "folderExists should" $ do
    it "return a recursive folder existing check chain" $ do
      folderExists "/one/two" `shouldBe`
        States [FolderExists "/one/two"]
          [ States [FolderExists "/one"]
            [ State [Not (DoesExist "/one")] [Remove "/one"] (Just "delete folder : /one")
            , State [Not AlwaysYes] [CreateFolder "/one"] (Just "create /one")] Nothing
          , States [FolderExists "/one/two"]
            [ State [Not (DoesExist "/one/two")] [Remove "/one/two"] (Just "delete folder : /one/two")
            , State [Not AlwaysYes] [CreateFolder "/one/two"] (Just "create /one/two")] Nothing]
        Nothing
      folderExists "/one/two/three" `shouldBe`
        States [FolderExists "/one/two/three"]
          [ States [FolderExists "/one"]
            [ State [Not (DoesExist "/one")] [Remove "/one"] (Just "delete folder : /one")
            , State [Not AlwaysYes] [CreateFolder "/one"] (Just "create /one")] Nothing
          , States [FolderExists "/one/two"]
            [ State [Not (DoesExist "/one/two")] [Remove "/one/two"] (Just "delete folder : /one/two")
            , State [Not AlwaysYes] [CreateFolder "/one/two"] (Just "create /one/two")] Nothing
          , States [FolderExists "/one/two/three"]
            [ State [Not (DoesExist "/one/two/three")] [Remove "/one/two/three"] (Just "delete folder : /one/two/three")
            , State [Not AlwaysYes] [CreateFolder "/one/two/three"] (Just "create /one/two/three")] Nothing]
        Nothing
      folderExists "~/one/two/three" `shouldBe`
        States [FolderExists "~/one/two/three"]
          [ States [FolderExists "~/one"]
            [ State [Not (DoesExist "~/one")] [Remove "~/one"] (Just "delete folder : ~/one")
            , State [Not AlwaysYes] [CreateFolder "~/one"] (Just "create ~/one")] Nothing
          , States [FolderExists "~/one/two"]
            [ State [Not (DoesExist "~/one/two")] [Remove "~/one/two"] (Just "delete folder : ~/one/two")
            , State [Not AlwaysYes] [CreateFolder "~/one/two"] (Just "create ~/one/two")] Nothing
          , States [FolderExists "~/one/two/three"]
            [ State [Not (DoesExist "~/one/two/three")] [Remove "~/one/two/three"] (Just "delete folder : ~/one/two/three")
            , State [Not AlwaysYes] [CreateFolder "~/one/two/three"] (Just "create ~/one/two/three")] Nothing]
        Nothing

  describe "basic functions" $ do
    it "testing should be set up properly" $ do
      removePathForcibly dynamicFolderPath
      folderType <- whatIsBehind' dynamicFolderPath
      folderType `shouldBe` DoesNotExist
    it "should create a file" $ do
      let file = (dynamicSubPath "file")
      executeIt [content file ["test"] ]
      folderType <- whatIsBehind' file
      folderType `shouldBe` IsFile
    it "should create a folder" $ do
      let folder = (dynamicSubPath "folder")
      executeIt [folderExists folder]
      folderType <- whatIsBehind' folder
      folderType `shouldBe` IsFolder
    it "should create a folder even if it is a file" $ do
      let folder = (dynamicSubPath "should/be_a_folder")
      executeIt [content folder ["test"]]
      executeIt [folderExists folder]
      folderType <- whatIsBehind' folder
      folderType `shouldBe` IsFolder
    it "should create a file even if it is a folder" $ do
      let file = (dynamicSubPath "should/be_a_file")
      executeIt [folderExists file]
      executeIt [content file ["test"] ]
      folderType <- whatIsBehind' file
      folderType `shouldBe` IsFile

  describe "preProcessState should" $ do
    it "replace ~ in paths" $ do
      home <- getHomeDirectory
      home `shouldNotBe` "~"
      state <- prePorcessState (UnixSystem verbosity) $ State [FolderExists "~/test"] [CreateFolder "~/test"] Nothing
      state `shouldBe` State [FolderExists $ home ++ "/test"] [CreateFolder $ home ++ "/test"] Nothing

  -- run
  describe "run once should" $ do
    it "create a file for a successful command" $ do
      beforeFile <- whatIsBehind' $ dynamicSubPath "run-once-success"
      beforeFile `shouldBe` DoesNotExist
      executeIt [run (Once "exit" ["0"] $ dynamicSubPath "run-once-success")]
      resultFile <- whatIsBehind' $ dynamicSubPath "run-once-success"
      resultFile `shouldBe` IsFile
    it "create no file for a failed command" $ do
      beforeFile <- whatIsBehind' $ dynamicSubPath "run-once-failure"
      beforeFile `shouldBe` DoesNotExist
      executeIt [run (Once "exit" ["1"] $ dynamicSubPath "run-once-failure")]
      resultFile <- whatIsBehind' $ dynamicSubPath "run-once-failure"
      resultFile `shouldBe` DoesNotExist
  describe "run which-creates should" $ do
    it "should run if outcome file does not exist" $ do
      beforeFile <- whatIsBehind' $ dynamicSubPath "run-which-creates-success"
      beforeFile `shouldBe` DoesNotExist
      executeIt [run (WithResults "touch" [dynamicSubPath "run-which-creates-success" ] $ [ Creates $ dynamicSubPath "run-which-creates-success" ])]
      resultFile <- whatIsBehind' $ dynamicSubPath "run-which-creates-success"
      resultFile `shouldBe` IsFile
    it "should not run if outcome file does exist" $ do
      beforeFile <- whatIsBehind' $ dynamicSubPath "run-which-creates-success"
      beforeFile `shouldBe` IsFile
      executeIt [run (WithResults "touch" [dynamicSubPath "run-which-creates-failure" ] $ [ Creates $ dynamicSubPath "run-which-creates-success" ] )]
      resultFile <- whatIsBehind' $ dynamicSubPath "run-which-creates-failure"
      resultFile `shouldBe` DoesNotExist



executeIt :: [State] -> IO ()
executeIt states = execute (LocalContext $ UnixSystem verbosity) states

dynamicFolderPath :: String
dynamicFolderPath = "./dynamic-test-folder"

dynamicSubPath :: String -> String
dynamicSubPath path = dynamicFolderPath ++ "/" ++ path

folderPath :: String
folderPath = "./test-resources/folder"

filePath :: String
filePath = "./test-resources/file"

linkPath :: String
linkPath = "./test-resources/filelink"

folderLinkPath :: String
folderLinkPath = "./test-resources/folderlink"
