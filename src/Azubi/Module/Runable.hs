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

import Azubi.Core.Model

-- todo : Syntax belongs to core
import Azubi.Syntax
import System.FilePath.Posix

{-|

creates a 'State' that will run a command of your choice.

-}
run :: RunCommand -> State
run (Once command arguments result) =
  let
    fullCommand = unwords $ command : arguments
    fileContent = [ "This is a Azubi cache file"
                  , "You can delete or edit it to make the following command run again"
                  , ""
                  , fullCommand ]
  in
    States
    [ HasFileContent result fileContent ]
    [ folderExists (takeDirectory result)
    , State [ SkipChecks ]
      [ Run command arguments $ Just $ unwords $ [ "run command" , command ] ++ arguments
      , FileContent result fileContent ]
      Nothing
    ]
    (Just $ "run once " ++ command ++ " " ++ (show arguments))

run (Always command arguments) =
  State
  [ SkipChecks ]
  [ Run command arguments $ Just $ unwords $ [ "run command",   command ] ++ arguments ]
  (Just $ "run always " ++ command ++ " " ++ (show arguments))


run (WithResults command arguments results) =
  States
  ( map translate results )
  [ State
    [ SkipChecks ]
    [ Run command arguments $ Just $ unwords $ [ "run command" , command ] ++ arguments ]
    Nothing
  ]
  (Just $ "run " ++ command ++ " " ++ (show arguments))
  where
    translate :: RunResults -> Check
    translate (Creates path) = DoesExist path
    translate (Deletes path) = Not $ DoesExist path




{-|

The way a command should be run. See 'run'

-}
data RunCommand =
  -- | run command every time
  Always String [Argument]
  -- | run command and creates a file to prevent to run again
  | Once String [Argument] Path
  -- | run command, with results
  -- See 'RunResults' on which results are possible
  | WithResults String [Argument] [RunResults]

data RunResults =
  -- | The command will create a file or folder
  -- if there is nothing behind this path
  -- this will trigger the command to be run.
  Creates Path
  -- | The command will delete a file or folder
  -- if there is there something behind the path
  -- this will trigger the command to be run.
  | Deletes Path


