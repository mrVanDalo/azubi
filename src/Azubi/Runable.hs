{-|

Module      : Azubi.Runable
Description : deploy 'run' command
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

-}
module Azubi.Runable where

import Azubi.Model


{-|

run a command

-}
data RunCommand =
  -- | run command every time
  Always String [Argument]
  -- | run command and creates a file to prevent to run again
  | Once String [Argument] Path


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
    Nothing

run (Always command arguments) =
  State
  [ Not AlwaysYes ]
  [ Run command arguments $ Just $ unwords $ [ "run command",   command ] ++ arguments ]
  Nothing
