
{-|

Module      : Azubi.Core.StateExecutors.LocalUnixStateExecutor
Description : 'StateExecutor' for Unix machines
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

Run 'State's on a Unix machine.

-}

module Azubi.Core.StateExecutors.LocalUnixStateExecutor where

import Azubi.Core.Model
import Azubi.Core.StateExecutor

import System.Directory

import System.Process hiding (runCommand)
import System.Exit

import System.Posix.Files (createSymbolicLink)

import System.FilePath.Posix

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput



data Verbosity = Verbose | Silent

{-|

Unix System like Linux, AIX or OSX

<https://en.wikipedia.org/wiki/Unix>

-}
data UnixSystem = UnixSystem { verbose :: Verbosity }


data PreProcessors =
  PreProcessors { homeUpdate :: (String -> String) }

homeReplacement :: String -> String -> String
homeReplacement home path =
  if ((head (splitDirectories path)) == "~")
  then do
    joinPath $ home : (drop 1 $ splitDirectories path)
  else
    path


instance LocalStateExecute UnixSystem where

  prePorcessState _ (State checks commands comment) = do
    home <- getHomeDirectory
    let preProcessors = PreProcessors (homeReplacement home)
    let newChecks =  (map (prePorcessCheck preProcessors) checks)
    let newCommands = (map (preProcessCommand preProcessors) commands)
    return $ State newChecks newCommands comment

  prePorcessState systemConfig (States checks states comment) = do
    home <- getHomeDirectory
    let preProcessors = PreProcessors (homeReplacement home)
    let newChecks = (map (prePorcessCheck preProcessors) checks)
    newStates <-  sequence $ map (prePorcessState systemConfig) states
    return $ States newChecks newStates comment


  executeState systemConfig (State checks commands comment) = do
    stateComment' comment
    checkResult <- collectCheckResults systemConfig checks
    case checkResult of
      Yes -> return Fulfilled
      No -> do
        commandResult <- collectRunResults systemConfig commands
        case commandResult of
          Success -> return Fulfilled
          Failure -> return Unfulfilled

  executeState systemConfig (States check states comment) = do
    stateComment' comment
    result <- collectCheckResults systemConfig check
    case result of
      Yes -> return Fulfilled
      No -> collectStateResults states
    where
      collectStateResults :: [State] -> IO StateResult
      collectStateResults [] = return Fulfilled
      collectStateResults (x:xs) = do
        result <- executeState systemConfig x
        case result of
          Unfulfilled -> return Unfulfilled
          Fulfilled -> collectStateResults xs

preProcessCommand ::PreProcessors -> Command -> Command
preProcessCommand _ (Run command arguments comment) =
  Run command arguments comment
preProcessCommand preprocessors (FileContent path content) =
  FileContent
  ((homeUpdate preprocessors) path)
  content
preProcessCommand preprocessors (CreateSymlink path target) =
  CreateSymlink
  ((homeUpdate preprocessors) path)
  ((homeUpdate preprocessors) target)
preProcessCommand preprocessors (CreateFolder path) =
  CreateFolder
  ((homeUpdate preprocessors) path)

preProcessCommand preprocessors (Remove path) =
  Remove
  ((homeUpdate preprocessors) path)

prePorcessCheck :: PreProcessors -> Check -> Check
prePorcessCheck _ (Check command arguments comment) =
  Check command arguments comment
prePorcessCheck _ AlwaysYes =
  AlwaysYes
prePorcessCheck preprocessors (Not check) =
  Not (prePorcessCheck preprocessors check)
prePorcessCheck preprocessors (HasFileContent path content) =
  HasFileContent
  ((homeUpdate preprocessors) path)
  content
prePorcessCheck preprocessors (SymlinkExists path target) =
  SymlinkExists
  ((homeUpdate preprocessors) path)
  ((homeUpdate preprocessors) target)
prePorcessCheck preprocessors (FolderExists path) =
  FolderExists
  ((homeUpdate preprocessors) path)
prePorcessCheck preprocessors (DoesExist path) =
  DoesExist
  ((homeUpdate preprocessors) path)


-- | unroll a number of Check(s)
-- | If one fail, they all fail
collectCheckResults :: UnixSystem -> [Check] -> IO CheckResult
collectCheckResults _ [] = return Yes
collectCheckResults systemConfig (check:rest) = do
        result <- runCheck systemConfig check
        case result of
          Yes -> collectCheckResults systemConfig rest
          No -> return No

-- | unroll a number of Run Commands
-- | if one fail, they all fail
collectRunResults :: UnixSystem -> [Command] -> IO CommandResult
collectRunResults _ [] = return Success
collectRunResults systemConfig (command:rest) = do
  result <- runCommand systemConfig command
  case result of
    Success -> collectRunResults systemConfig rest
    Failure -> return Failure

-- | Run a command
runCommand :: UnixSystem -> Command -> IO CommandResult
runCommand systemConfig (CreateFolder path') = do
  path <- goodPath path'
  logger' systemConfig commandComment' ["create directory ", path]
  createDirectoryIfMissing True path
  return Success

runCommand systemConfig (FileContent path' content) = do
  path <- goodPath path'
  logger' systemConfig commandComment' ["write content to ", path]
  writeFile path $ unlines content
  return Success

runCommand systemConfig (CreateSymlink path' target) = do
  path <- goodPath path'
  logger' systemConfig commandComment' ["create link", path, " to ", target]
  createSymbolicLink target path
  return Success

runCommand systemConfig (Run command arguments comment) = do
  commandComment' comment
  logger' systemConfig commandComment' ["run shell command", command, show arguments] 
  result <- runProcess' systemConfig [command] arguments
  case result of
    ExitSuccess -> return Success
    _ -> return Failure

runCommand systemConfig (Remove path') = do
  path <- goodPath path'
  logger' systemConfig commandComment' ["remove", path]
  removePathForcibly path
  return Success


-- | Run a Check
runCheck :: UnixSystem -> Check -> IO CheckResult
runCheck systemConfig (FolderExists path) = do
  behind <- whatIsBehind' path
  case behind of
    IsFolder -> do
      logger' systemConfig checkComment' ["FolderExists", path, ": YES"]
      return Yes
    _ -> do
      logger' systemConfig checkComment' ["FolderExists", path, ": NO"]
      return No

runCheck systemConfig (SymlinkExists path target) = do
  goodTarget <- goodPath target
  behind <- whatIsBehind' path
  case behind of
    IsSymlink behindTarget -> do
      if behindTarget == goodTarget
      then do
        logger' systemConfig checkComment' ["SymlinkExists", path, "->", target, ": YES"]
        return Yes
      else do
        logger' systemConfig checkComment' ["SymlinkExists", path, "->", target, ": NO"]
        return No
    _ -> do
      logger' systemConfig checkComment' ["SymlinkExists", path, "->", target, ": NO"]
      return No

runCheck systemConfig (HasFileContent path' content) = do
  path <- goodPath path'
  behind <- whatIsBehind' path
  case behind of
    IsFile -> checkContent
    _ -> do
      logger' systemConfig checkComment' ["HasFileContent", path, ": NO"]
      return No
  where
    checkContent = do
      path <- goodPath path'
      file <- readFile path
      currentContent <- return $ lines file
      diff <- return $ getGroupedDiff currentContent content
      case diff of
        (Both _ _):[] -> do
          logger' systemConfig checkComment' ["HasFileContent", path, ": YES"]
          return Yes
        _ -> do
          logger' systemConfig checkComment' ["HasFileContent", path, ": NO"]
          echo' [ppDiff diff]
          return No

runCheck systemConfig (Check command args comment) = do
  checkComment' comment
  result <- runProcess' systemConfig [command] args
  case result of
    ExitSuccess -> do
      logger' systemConfig checkComment' ["Shell Command Check", command , show args , ": YES"]
      return Yes
    _ -> do
      logger' systemConfig checkComment' ["Shell Command Check", command , show args , ": NO"]
      return No

runCheck systemConfig  (Not check ) = do
  result <- runCheck systemConfig check
  case result of
    No -> return Yes
    Yes  -> return No

runCheck _ AlwaysYes = return Yes

runCheck systemConfig (DoesExist path) = do
  behind <- whatIsBehind' path
  case behind of
    DoesNotExist -> do
      logger' systemConfig checkComment' ["DoesExist",path, ": NO"]
      return No
    _ -> do
      logger' systemConfig checkComment' ["DoesExist",path, ": YES"]
      return Yes


data FileType = IsFile
              | DoesNotExist
              | IsSymlink Path
              | IsFolder
              deriving (Show, Eq)

-- | helper function to check whats behind a path
whatIsBehind' :: String -> IO FileType
whatIsBehind' path' = do
  path <- goodPath path'
  exists <- doesPathExist path
  if exists
    then figureOutFileType path
    else return DoesNotExist
  where
    figureOutFileType path = do
      checkFolder <- doesDirectoryExist path
      checkSymlink <- pathIsSymbolicLink path
      case (checkSymlink, checkFolder) of
        (True, _) -> do
            target <- getSymbolicLinkTarget path
            goodTarget <- goodPath target
            return $ IsSymlink goodTarget
        (False, True) -> return IsFolder
        (False, False) -> return IsFile

{-|

run a process and wait until it's finished
return the exit code

-}
runProcess' :: UnixSystem -> [String] -> [String] -> IO ExitCode
runProcess' systemConfig command args =  do
  (_, _ , _ , checkHandle ) <- createProcess (shell $ unwords $ command ++ args ){ std_out = stdOutHandle }
  waitForProcess checkHandle
  where
    stdOutHandle :: StdStream
    stdOutHandle =
      case (verbose systemConfig) of
        Verbose -> Inherit
        Silent -> NoStream

{-|

corrects the path

* replaces ~

-}
goodPath :: String -> IO String
goodPath path = if ((head (splitDirectories path)) == "~")
     then do
       home <- getHomeDirectory
       return $ joinPath $ home : (drop 1 (splitDirectories path))
     else
       return path


-- | simple print function
echo' :: [String] -> IO ()
echo' text = putStrLn $ unwords $ "[Azubi]":text


-- | render state comments
stateComment' :: Maybe Comment -> IO ()
stateComment' (Just comment) = echo' ["[State]", comment]
stateComment' Nothing = return ()

-- | render command comments
commandComment' :: Maybe Comment -> IO ()
commandComment' (Just comment) = echo' ["[Run]", comment]
commandComment' Nothing = return ()

-- | render check comments
checkComment' :: Maybe Comment -> IO ()
checkComment' (Just comment) = echo' ["[Check]", comment]
checkComment' Nothing = return ()

logger' :: UnixSystem -> (Maybe Comment -> IO ()) -> [Comment] -> IO ()
logger' _ _ [] = return ()
logger' systemConfig messager comment =
  case (verbose systemConfig) of
    Verbose -> messager $ Just $ unwords comment
    Silent -> return ()

