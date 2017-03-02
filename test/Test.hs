
import Test.Hspec

import Azubi.Syntax
import Azubi.Core.Model
import Azubi.Core.StateExecutors.LocalUnixStateExecutor

main :: IO ()
main = hspec $ do
  describe "requires" $ do
    it "should be the same as submoulde for 2 arguments" $ do
       (stateA `requires` stateB) `shouldBe` (submodule [stateB, stateA])

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



folderPath :: String
folderPath = "./test-resources/folder"

filePath :: String
filePath = "./test-resources/file"

linkPath :: String
linkPath = "./test-resources/filelink"

folderLinkPath :: String
folderLinkPath = "./test-resources/folderlink"

stateA :: State
stateA = State [] [Run "echo" ["you"] Nothing] Nothing
stateB :: State
stateB = State [] [Run "echo" ["me"] Nothing] Nothing
