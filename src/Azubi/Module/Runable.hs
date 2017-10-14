{-|

Module      : Azubi.Module.Runable
Description : deploy 'run' command
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

-}
module Azubi.Module.Runable where

import           Azubi.Core.Model

import           Azubi.Syntax
import           System.FilePath.Posix

{-|

creates a 'State' that will run a command of your choice.

-}
run :: RunCommand -> State
run (Once commandToRun argumentsOfCommand result) =
  let fullCommand = unwords $ commandToRun : argumentsOfCommand
      cacheFileContent =
        [ "This is a Azubi cache file"
        , "You can delete or edit it to make the following command run again"
        , ""
        , fullCommand
        ]
  in States
       [HasFileContent result cacheFileContent]
       [ folderExists (takeDirectory result)
       , State
           [SkipChecks]
           [ Run commandToRun argumentsOfCommand $
             Just $
             unwords $ ["run command", commandToRun] ++ argumentsOfCommand
           , FileContent result cacheFileContent
           ]
           Nothing
       ]
       (Just $ unwords ["run once ", commandToRun, " ", show argumentsOfCommand])
run (Always commandToRun argumentsOfCommand) =
  State
    [SkipChecks]
    [ Run commandToRun argumentsOfCommand $
      Just $ unwords $ ["run command", commandToRun] ++ argumentsOfCommand
    ]
    (Just $ unwords ["run always ", commandToRun, " ", show argumentsOfCommand])
run (WithResults commandToRun argumentsOfCommand results) =
  States
    (map translate results)
    [ State
        [SkipChecks]
        [ Run commandToRun argumentsOfCommand $
          Just $ unwords $ ["run command", commandToRun] ++ argumentsOfCommand
        ]
        Nothing
    ]
    (Just $ unwords ["run ", commandToRun, " ", show argumentsOfCommand])
  where
    translate :: RunResult -> Check
    translate (Creates pathToCreate) = DoesExist pathToCreate
    translate (Deletes pathToDelete) = Not $ DoesExist pathToDelete

{-|

The way a command should be run. See 'run'

-}
data RunCommand
  -- | run command every time
  = Always { runCommand   :: String
           , runArguments :: [String] }
  -- | run command and creates a file to prevent to run again
  | Once { runCommand   :: String
         , runArguments :: [String]
         , cachePath    :: String }
  -- | run command, with results
  -- See 'RunResults' on which results are possible
  | WithResults { runCommand   :: String
                , runArguments :: [String]
                , runResults   :: [RunResult] }

data RunResult
  -- | The command will create a file or folder
  -- if there is nothing behind this path
  -- this will trigger the command to be run.
  = Creates Path
  -- | The command will delete a file or folder
  -- if there is there something behind the path
  -- this will trigger the command to be run.
  | Deletes Path
