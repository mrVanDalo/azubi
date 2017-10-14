{-|

Module      : Azubi.Core.Interpreter.LocalUnixInterpreter
Description : 'Interpreter' for Unix machines
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

Run 'State's on a Unix machine.

-}
module Azubi.Core.Interpreter.LocalUnixInterpreter where

import           Azubi.Core.Model
import           Azubi.Core.Interpreter

import           System.Directory

import           System.Exit
import           System.Process                      hiding ( runCommand )

import           System.Posix.Files                  ( createSymbolicLink )

import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput

import qualified Azubi.Core.Interpreter.UnixUtils as Util

{-|

Unix System like Linux, AIX or OSX

<https://en.wikipedia.org/wiki/Unix>

-}
newtype UnixSystem = UnixSystem
  { verbose :: Verbosity
  }

data Verbosity
  = Verbose
  | Silent

instance LocalInterpreter UnixSystem where
  prePorcessState _ state@State {stateChecks = checks, stateCommands = commands} = do
    preProcessors <- Util.preProcessors
    let checkPreprocessor = prePorcessCheck preProcessors
    let commandPreprocessor = preProcessCommand preProcessors
    let processedChecks = map checkPreprocessor checks
    let processedCommands = map commandPreprocessor commands
    return
      state {stateChecks = processedChecks, stateCommands = processedCommands}
  prePorcessState systemConfig states@States { stateChecks = checksToPreprocess
                                             , subStates = statesToPreProcess
                                             } = do
    preProcessors <- Util.preProcessors
    let checkPreprocessor = prePorcessCheck preProcessors
    let statePreprocessor = prePorcessState systemConfig
    let processedChecks = map checkPreprocessor checksToPreprocess
    processedSubStates <- mapM statePreprocessor statesToPreProcess
    return
      states {stateChecks = processedChecks, subStates = processedSubStates}
  executeState systemConfig State { stateChecks = checks
                                  , stateCommands = commands
                                  , stateComment = comment
                                  } = do
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
      No  -> collectStateResults states
    where
      collectStateResults :: [State] -> IO StateResult
      collectStateResults [] = return Fulfilled
      collectStateResults (x:xs) = do
        result <- executeState systemConfig x
        case result of
          Unfulfilled -> return Unfulfilled
          Fulfilled   -> collectStateResults xs

preProcessCommand :: Util.PreProcessors -> Command -> Command
preProcessCommand _ run@Run {} = run
preProcessCommand preProcessors content@FileContent {filePath = path} =
  content {filePath = Util.homeUpdate preProcessors path}
preProcessCommand preProcessors (CreateSymlink path target) =
  CreateSymlink
    (Util.homeUpdate preProcessors path)
    (Util.homeUpdate preProcessors target)
preProcessCommand preProcessors (CreateFolder path) =
  CreateFolder (Util.homeUpdate preProcessors path)
preProcessCommand preProcessors (Remove path) =
  Remove (Util.homeUpdate preProcessors path)

prePorcessCheck :: Util.PreProcessors -> Check -> Check
prePorcessCheck _ check@Check {} = check
prePorcessCheck _ SkipChecks = SkipChecks
prePorcessCheck preProcessors (Not check) =
  Not (prePorcessCheck preProcessors check)
prePorcessCheck preProcessors content@HasFileContent {pathToCheck = path} =
  content {pathToCheck = Util.homeUpdate preProcessors path}
prePorcessCheck preProcessors (SymlinkExists path target) =
  SymlinkExists
    (Util.homeUpdate preProcessors path)
    (Util.homeUpdate preProcessors target)
prePorcessCheck preProcessors (FolderExists path) =
  FolderExists (Util.homeUpdate preProcessors path)
prePorcessCheck preProcessors (DoesExist path) =
  DoesExist (Util.homeUpdate preProcessors path)

-- | unroll a number of Check(s)
-- | If one fail, they all fail
collectCheckResults :: UnixSystem -> [Check] -> IO CheckResult
collectCheckResults _ [] = return Yes
collectCheckResults systemConfig (check:rest) = do
  result <- runCheck systemConfig check
  case result of
    Yes -> collectCheckResults systemConfig rest
    No  -> return No

-- | unroll a number of Run Commands
-- | if one fail, they all fail
collectRunResults :: UnixSystem -> [Command] -> IO CommandResult
collectRunResults _ [] = return Success
collectRunResults systemConfig (commandToRun:rest) = do
  result <- runCommand systemConfig commandToRun
  case result of
    Success -> collectRunResults systemConfig rest
    Failure -> return Failure

-- | Run a command
runCommand :: UnixSystem -> Command -> IO CommandResult
runCommand systemConfig (CreateFolder path) = do
  logger' systemConfig commandComment' ["create directory ", path]
  createDirectoryIfMissing True path
  return Success
runCommand systemConfig (FileContent path content) = do
  logger' systemConfig commandComment' ["write content to ", path]
  writeFile path $ unlines content
  return Success
runCommand systemConfig (CreateSymlink path target) = do
  logger' systemConfig commandComment' ["create link", path, " to ", target]
  createSymbolicLink target path
  return Success
runCommand systemConfig (Run commandToRun arguments comment) = do
  commandComment' comment
  logger'
    systemConfig
    commandComment'
    ["run shell command", commandToRun, show arguments]
  result <- runProcess' systemConfig commandToRun arguments
  case result of
    ExitSuccess -> return Success
    _           -> return Failure
runCommand systemConfig (Remove path) = do
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
  behind <- whatIsBehind' path
  case behind of
    IsSymlink behindTarget ->
      if behindTarget == target
        then do
          logger'
            systemConfig
            checkComment'
            ["SymlinkExists", path, "->", target, ": YES"]
          return Yes
        else do
          logger'
            systemConfig
            checkComment'
            ["SymlinkExists", path, "->", target, ": NO"]
          return No
    _ -> do
      logger'
        systemConfig
        checkComment'
        ["SymlinkExists", path, "->", target, ": NO"]
      return No
runCheck systemConfig (HasFileContent path content) = do
  behind <- whatIsBehind' path
  case behind of
    IsFile -> checkContent
    _ -> do
      logger' systemConfig checkComment' ["HasFileContent", path, ": NO"]
      return No
  where
    checkContent = do
      file <- readFile path
      let currentContent = lines file
      let diff = getGroupedDiff currentContent content
      case diff of
        [Both _ _] -> do
          logger' systemConfig checkComment' ["HasFileContent", path, ": YES"]
          return Yes
        _ -> do
          logger' systemConfig checkComment' ["HasFileContent", path, ": NO"]
          echo' [ppDiff diff]
          return No
runCheck systemConfig (Check commandToRun args comment) = do
  checkComment' comment
  result <- runProcess' systemConfig commandToRun args
  case result of
    ExitSuccess -> do
      logger'
        systemConfig
        checkComment'
        ["Shell Command Check", commandToRun, show args, ": YES"]
      return Yes
    _ -> do
      logger'
        systemConfig
        checkComment'
        ["Shell Command Check", commandToRun, show args, ": NO"]
      return No
runCheck systemConfig (Not check) = do
  result <- runCheck systemConfig check
  case result of
    No  -> return Yes
    Yes -> return No
-- returns `No` to make the State run always the Commands
runCheck _ SkipChecks = return No
runCheck systemConfig (DoesExist path) = do
  behind <- whatIsBehind' path
  case behind of
    DoesNotExist -> do
      logger' systemConfig checkComment' ["DoesExist", path, ": NO"]
      return No
    _ -> do
      logger' systemConfig checkComment' ["DoesExist", path, ": YES"]
      return Yes

data FileType
  = IsFile
  | DoesNotExist
  | IsSymlink Path
  | IsFolder
  deriving (Show, Eq)

-- | helper function to check whats behind a path
whatIsBehind' :: String -> IO FileType
whatIsBehind' path' = do
  preProcessors <- Util.preProcessors
  let path = Util.homeUpdate preProcessors path'
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
          return $ IsSymlink target
        (False, True) -> return IsFolder
        (False, False) -> return IsFile

{-|

run a process and wait until it's finished
return the exit code

-}
runProcess' :: UnixSystem -> String -> [String] -> IO ExitCode
runProcess' systemConfig commandToRun args = do
  (_, _, _, checkHandle) <- createProcess shellProcess {std_out = stdOutHandle}
  waitForProcess checkHandle
  where
    shellCommand = unwords $ commandToRun : args
    shellProcess = shell shellCommand
    stdOutHandle :: StdStream
    stdOutHandle =
      case verbose systemConfig of
        Verbose -> Inherit
        Silent  -> NoStream

-- | simple print function
echo' :: [String] -> IO ()
echo' text = putStrLn $ unwords $ "[Azubi]" : text

-- | render state comments
stateComment' :: Maybe Comment -> IO ()
stateComment' (Just comment) = echo' ["[State]", comment]
stateComment' Nothing        = return ()

-- | render command comments
commandComment' :: Maybe Comment -> IO ()
commandComment' (Just comment) = echo' ["[Run]", comment]
commandComment' Nothing        = return ()

-- | render check comments
checkComment' :: Maybe Comment -> IO ()
checkComment' (Just comment) = echo' ["[Check]", comment]
checkComment' Nothing        = return ()

logger' :: UnixSystem -> (Maybe Comment -> IO ()) -> [Comment] -> IO ()
logger' _ _ [] = return ()
logger' systemConfig messager comment =
  case verbose systemConfig of
    Verbose -> messager $ Just $ unwords comment
    Silent  -> return ()
