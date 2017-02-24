
{-|

Module      : Azubi.StateExecutors.LocalUnixStateExecutor
Description : 'StateExecutor' for Unix machines
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

Run 'State's on a Unix machine.

-}

module Azubi.StateExecutors.LocalUnixStateExecutor ( UnixSystem(..) ) where

import Azubi.Model
import Azubi.StateExecutor

import System.Directory
import System.FilePath

import System.Process
import System.Exit

import System.Posix.Files (createSymbolicLink)

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput



{-|

Unix System like Linux, AIX or OSX

<https://en.wikipedia.org/wiki/Unix>

-}
data UnixSystem = UnixSystem

instance LocalStateExecute UnixSystem where
  setup UnixSystem = return ()
  tearDown UnixSystem stateResults = return ()


  run UnixSystem (State checks commands comment) = do
    runComment' comment
    checkResult <- collectCheckResults checks
    case checkResult of
      Yes -> return Fulfilled
      No -> do
        commandResult <- collectRunResults commands
        case commandResult of
          Success -> return Fulfilled
          Failure -> return Unfulfilled

  run UnixSystem (States states comment) = do
    runComment' comment
    collectStateResults states
    where
      collectStateResults :: [State] -> IO StateResult
      collectStateResults [] = return Fulfilled
      collectStateResults (x:xs) = do
        result <- run UnixSystem x
        case result of
          Unfulfilled -> return Unfulfilled
          Fulfilled -> collectStateResults xs

-- | unroll a number of Check(s)
-- | If one fail, they all fail
collectCheckResults :: [Check] -> IO CheckResult
collectCheckResults [] = return Yes
collectCheckResults (check:rest) = do
        result <- runCheck' check
        case result of
          Yes -> collectCheckResults rest
          No -> return No

-- | unroll a number of Run Commands
-- | if one fail, they all fail
collectRunResults :: [Command] -> IO CommandResult
collectRunResults [] = return Success
collectRunResults (command:rest) = do
        result <- runCommand' command
        case result of
          Success -> collectRunResults rest
          Failure -> return Failure

-- | Run a command
runCommand' :: Command -> IO CommandResult
runCommand' (CreateFolder path) = do
        createDirectoryIfMissing True path
        -- todo check if it really worked
        return Success

runCommand' (FileContent path content) = do
        writeFile path $ unlines content
        -- todo check it really worked
        return Success

runCommand' (CreateSymlink path target) = do
  -- todo check results
  -- todo this only works in a unix environment
  createSymbolicLink target path
  return Success

runCommand' (Run command arguments comment) = do
  runComment' comment
  result <- runProcess' [command] arguments
  case result of
    ExitSuccess -> return Success
    _ -> return Failure


-- | show a comment
runComment' :: Maybe Comment -> IO ()
runComment' (Just comment) = echo' [comment]
runComment' Nothing = return ()



-- | Run a Check
runCheck' :: Check -> IO CheckResult
runCheck' (FolderExists path) = do
  behind <- whatIsBehind' path
  case behind of
    IsFolder -> return Yes
    _ -> return No

-- todo check target of the symlink
runCheck' (SymlinkExists path target) = do
  behind <- whatIsBehind' path
  case behind of
    IsSymlink -> return Yes
    _ -> return No

runCheck' (HasFileContent path content) = do
  behind <- whatIsBehind' path
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
runCheck' (Check command args comment) = do
  runComment' comment
  result <- runProcess' [command] args
  case result of
    ExitSuccess -> return Yes
    _ -> return No

runCheck' (NotCheck command args comment) = do
  runComment' comment
  result <- runProcess' [command] args
  case result of
    ExitSuccess -> return No
    _ -> return Yes



data FileType = IsFile
              | DoesNotExist
              | IsSymlink
              | IsFolder
              deriving (Show, Eq, Enum)

-- | helper function to check whats behind a path
whatIsBehind' :: String -> IO FileType
whatIsBehind' path = do
  checkFile <- doesFileExist path
  checkFolder <- doesDirectoryExist path
  case (checkFolder, checkFile) of
    (True,  _) -> return IsFolder
    (False, True) -> do
      checkSymlink <- pathIsSymbolicLink path
      if checkSymlink
      then return IsSymlink
      else return IsFile
    (False, False) -> return DoesNotExist


-- | simple print function
echo' :: [String] -> IO ()
echo' text = putStrLn $ unwords $ "[Azubi]":text

-- | run a process and wait until it's finished
-- | return the exit code
runProcess' :: [String] -> [String] -> IO ExitCode
runProcess' command args =  do
  -- todo : pipe to /dev/null
  (_, _, _, checkHandle ) <- createProcess (shell $ unwords $ command ++ args )
  -- todo : the comment is ignored
  waitForProcess checkHandle


