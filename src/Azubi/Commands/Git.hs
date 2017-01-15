


module Azubi.Commands.Git where


import Azubi.Core.Command

import Azubi.Commands.Existance
import Azubi.Commands.File

import System.FilePath

data Repository = Git { repo :: String
                      , folder :: String
                      , options :: [GitOptions]}

data GitOptions = Branch String
                | Recursive

instance Existance Repository where

  doExists (Git repo folder options) con =
    [Dependency{
        body = bodyCommands
        , dependency = [IfCommand{
                           testCommand = BoolCommand $ "-d " ++ folder
                           , thenCommand = []
                           , elseCommand = (exists (Directory parentFolder) con)
                                           ++ [ InfoMsg $ "clone git repository " ++ repo ++ " to folder " ++ folder
                                              , ShellCommand $ unwords $ ["git", "clone"] ++ gitOptions ++ [repo, folder] ]
                           }]
        }]
    where
      parentFolder = takeDirectory folder
      gitOptions = concat $ map (\opt -> case opt of
                                    Recursive -> ["--recursive"]
                                    _ -> []
                       ) options
      bodyCommands = concat $ map (\opt -> case opt of
                                      Branch branch ->  [ ShellCommand $ "cd " ++ folder
                                                        , ShellCommand $ "git checkout " ++ branch]
                                      _ -> []
                                  ) options

  notExists (Git _ folder _) con = notExists (Directory folder) con

