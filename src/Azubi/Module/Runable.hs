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
    State
    [ HasFileContent result fileContent ]
    [ Run command arguments $ Just $ unwords $ [ "run command" , command ] ++ arguments
    , FileContent result fileContent ]
    (Just $ "run once " ++ command ++ " " ++ (show arguments))

run (Always command arguments) =
  State
  [ Not AlwaysYes ]
  [ Run command arguments $ Just $ unwords $ [ "run command",   command ] ++ arguments ]
  (Just $ "run always " ++ command ++ " " ++ (show arguments))

{-|

The way a command should be run. See 'run'

-}
data RunCommand =
  -- | run command every time
  Always String [Argument]
  -- | run command and creates a file to prevent to run again
  | Once String [Argument] Path


