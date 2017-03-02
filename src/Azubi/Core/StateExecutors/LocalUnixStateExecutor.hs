
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

import System.Process
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

instance LocalStateExecute UnixSystem where
  setup _ = return ()
  tearDown _ _ = return ()


  executeState systemConfig (State checks commands comment) = do
    runComment' comment
    checkResult <- collectCheckResults systemConfig checks
    case checkResult of
      Yes -> return Fulfilled
      No -> do
        commandResult <- collectRunResults systemConfig commands
        case commandResult of
          Success -> return Fulfilled
          Failure -> return Unfulfilled

  executeState systemConfig (States check states comment) = do
    runComment' comment
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

-- | unroll a number of Check(s)
-- | If one fail, they all fail
collectCheckResults :: UnixSystem -> [Check] -> IO CheckResult
collectCheckResults _ [] = return Yes
collectCheckResults systemConfig (check:rest) = do
        result <- runCheck' systemConfig check
        case result of
          Yes -> collectCheckResults systemConfig rest
          No -> return No

-- | unroll a number of Run Commands
-- | if one fail, they all fail
collectRunResults :: UnixSystem -> [Command] -> IO CommandResult
collectRunResults _ [] = return Success
collectRunResults systemConfig (command:rest) = do
  result <- runCommand' systemConfig command
  case result of
    Success -> collectRunResults systemConfig rest
    Failure -> return Failure

-- | Run a command
runCommand' :: UnixSystem -> Command -> IO CommandResult
runCommand' _ (CreateFolder path) = do
  runComment' $ Just $ "create directory " ++ path
  createDirectoryIfMissing True path
  return Success

runCommand' _ (FileContent path content) = do
  runComment' $ Just $ "write content to " ++ path
  writeFile path $ unlines content
  return Success

runCommand' _ (CreateSymlink path target) = do
  runComment' $ Just $ "create link " ++ path ++ " to " ++ target
  createSymbolicLink target path
  return Success

runCommand' systemConfig (Run command arguments comment) = do
  runComment' comment
  result <- runProcess' systemConfig [command] arguments
  case result of
    ExitSuccess -> return Success
    _ -> return Failure

runCommand' _ (Remove path) = do
  runComment' (Just $ "remove " ++ path)
  removePathForcibly path
  return Success


-- | show a comment
runComment' :: Maybe Comment -> IO ()
runComment' (Just comment) = echo' [comment]
runComment' Nothing = return ()



-- | Run a Check
runCheck' :: UnixSystem -> Check -> IO CheckResult
runCheck' systemConfig (FolderExists path) = do
  behind <- whatIsBehind' path
  log' systemConfig ["check if folder exists", (show behind)]
  case behind of
    IsFolder -> return Yes
    _ -> return No

runCheck' systemConfig (SymlinkExists path target) = do
  goodTarget <- goodPath' target
  behind <- whatIsBehind' path
  log' systemConfig ["check if symlink exists", path, target, (show behind) ]
  case behind of
    IsSymlink behindTarget -> do
      if behindTarget == goodTarget
      then return Yes
      else return No
    _ -> return No

runCheck' systemConfig (HasFileContent path content) = do
  behind <- whatIsBehind' path
  log' systemConfig ["check if file has content ", (show behind)]
  case behind of
    IsFile -> checkContent
    _ -> return No
  where
    checkContent = do
      file <- readFile path
      currentContent <- return $ lines file
      diff <- return $ getGroupedDiff currentContent content
      case diff of
        (Both _ _):[] -> return Yes
        _ -> do
            echo' [ppDiff diff]
            return No
runCheck' systemConfig (Check command args comment) = do
  runComment' comment
  log' systemConfig ["check command ", command , show args ]
  result <- runProcess' systemConfig [command] args
  case result of
    ExitSuccess -> return Yes
    _ -> return No

runCheck' systemConfig  (Not check ) = do
  result <- runCheck' systemConfig check
  case result of
    No -> return Yes
    Yes  -> return No

runCheck' _ AlwaysYes = return Yes

runCheck' _ (DoesExist path) = do
  behind <- whatIsBehind' path
  case behind of
    DoesNotExist -> return No
    _ -> return Yes


data FileType = IsFile
              | DoesNotExist
              | IsSymlink Path
              | IsFolder
              deriving (Show, Eq)

-- | helper function to check whats behind a path
whatIsBehind' :: String -> IO FileType
whatIsBehind' path' = do
  path <- goodPath' path'
  checkFile <- doesFileExist path
  checkFolder <- doesDirectoryExist path
  checkSymlink <- pathIsSymbolicLink path
  case (checkSymlink, checkFolder, checkFile) of
    (True, _, _ ) -> do
        target <- getSymbolicLinkTarget path
        goodTarget <- goodPath' target
        return $ IsSymlink goodTarget
    (False, True,  _) -> return IsFolder
    (False, False, True) -> return IsFile
    (False, False, False) -> return DoesNotExist


{-|

corrects the path

* replaces ~

-}

goodPath' :: String -> IO String
goodPath' path = if ((head (splitDirectories path)) == "~")
     then do
       home <- getHomeDirectory
       return $ joinPath $ home : (drop 1 (splitDirectories path))
     else
       return path


-- | simple print function
echo' :: [String] -> IO ()
echo' text = putStrLn $ unwords $ "[Azubi]":text

-- | simple log function
log' :: UnixSystem -> [String] -> IO ()
log' systemConfig message = case (verbose systemConfig) of
  Verbose -> echo' message
  Silent -> return ()

-- | run a process and wait until it's finished
-- | return the exit code
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


