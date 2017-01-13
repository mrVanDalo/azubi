
module Azubi.Commands.Run where

import Azubi.Core.Command
import Azubi.Core.Context

import Azubi.Commands.File



-- | run a command which creates Nothing or a File.
run:: (Context a) => String -> Maybe File -> a -> [Command]
run command (Just(File path)) _ = [ IfCommand {
                                      testCommand = BoolCommand $ "-e " ++ path
                                      , thenCommand = []
                                      , elseCommand = [ShellCommand command]
                                      }]

run command (Just(Directory path)) _ = [ IfCommand {
                                           testCommand = BoolCommand $ "-d " ++ path
                                           , thenCommand = []
                                           , elseCommand = [ShellCommand command]
                                           }]

run command (Just (Symlink path target)) _ = [ IfCommand {
                                                 testCommand = BoolCommand $ "-L " ++ path
                                                 , thenCommand = [] -- todo : check if link points to path
                                                 , elseCommand = [ShellCommand command]
                                                 }]

run command Nothing _ = [ InfoMsg $ "run " ++ command
                        , ShellCommand command
                        ]
