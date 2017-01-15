
module Azubi.Commands.File where

import Azubi.Core.Revertable
import Azubi.Core.Command

import Azubi.Commands.Existance

import System.FilePath

data File = File { path :: String }
          | Symlink { path :: String
                    , target :: String
                    }
          | Directory { path :: String }




-- ------------------------------------------------------------
--
-- exists / not exists
--
-- ------------------------------------------------------------
instance Existance File where
  doExists (File path) con =
    [ IfCommand (BoolCommand $ "-e " ++ path)
      [ InfoMsg $ path ++ " already exists" ]
      ((doExists ( Directory $ takeDirectory path ) con)
        ++ [ InfoMsg $ "check existence of " ++ path
           , ShellCommand $ "touch " ++  path ])
      ]

  doExists (Symlink path target) con =
    ( doExists ( Directory $ takeDirectory path ) con)
    ++ [ InfoMsg $ "create symlink " ++ path ++ " -> " ++ target
       , ShellCommand $ "ln -s -f --no-target-directory " ++ target ++ " " ++ path ]

  doExists (Directory path) con = [
    IfCommand {
        testCommand = BoolCommand $ "-d " ++ path
        , thenCommand = [ LogMsg $ "directory " ++ path ++ " exists"]
        , elseCommand = [ InfoMsg $ "create direcotory " ++ path
                        , ShellCommand $ "mkdir -p " ++ path ]
        }
    ]


  notExists (File path) con =
    [ IfCommand (BoolCommand $ "-e " ++ path)
      [ShellCommand $ "rm -f " ++ path ]
      [InfoMsg $ path ++ " does not exists"]
    ]

  notExists (Symlink path target) con = notExists (File path) con

  notExists  (Directory path) con =
    [ IfCommand (BoolCommand $ "-d " ++ path)
      [ ShellCommand $ "rm -fr " ++ path ]
      [ LogMsg $ path ++ " does not exists" ]
    ]



-- ------------------------------------------------------------
--
-- File Content
--
-- ------------------------------------------------------------

-- | todo : rename consists
-- |        contain should mean that a string inside exists like this. (sed magic than happens)
contains :: (Revertable a ) => File -> [ String ] -> a -> [ Command ]
contains file content revertable =
  if (isRevert revertable) then
    notExists file revertable
  else
    doContent file
  where
    doContent (File path) =
      exists file revertable
      ++ [ InfoMsg $ "write content to " ++ path
         , FileContent path content
         ]
    doContent (Symlink _ target) =
      exists file revertable
      ++ [ InfoMsg $ "write content to " ++ target
         , FileContent target content
         ]
    doContent (Directory path ) =
      exists file revertable
      ++ [ ErrorMsg $ "can not put content in a directory " ++ path ]
