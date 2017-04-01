
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

  describe "basic functions" $ do
    it "testing should be set up properly" $ do
      removePathForcibly dynamicFolderPath
      folderType <- whatIsBehind' dynamicFolderPath
      folderType `shouldBe` DoesNotExist
    it "should create a folder" $ do
      executeIt [folderExists dynamicFolderPath]
      folderType <- whatIsBehind' dynamicFolderPath
      folderType `shouldBe` IsFolder
    it "should create a file" $ do
      let file = (dynamicSubPath "file")
      executeIt [content file ["test"] ]
      folderType <- whatIsBehind' file
      folderType `shouldBe` IsFile
    it "should create a folder" $ do
      let folder = (dynamicSubPath "folder")
      executeIt [folderExists  folder]
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
