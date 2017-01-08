
module Commands.File where

import Core.Revertable
import Core.Command
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
exists :: (Revertable a) => File -> a -> [ Command ]
exists file revertable =
  if (isRevert revertable) then
    notexists file (toggleRevert revertable)
  else
    doExists file
  where
    doExists (File path) =
      [ IfCommand (BoolCommand $ "-e " ++ path)
        [ InfoMsg $ path ++ " already exists" ]
        ((doExists ( Directory $ takeDirectory path ))
          ++ [ InfoMsg $ "check existence of " ++ path
             , ShellCommand $ "touch " ++  path ])
      ]
    doExists (Symlink path target) =
      ( doExists ( Directory $ takeDirectory path ) )
      ++ [ InfoMsg $ "create symlink " ++ path ++ " -> " ++ target
         , ShellCommand $ "ln -s " ++ target ++ " " ++ path ]
    doExists (Directory path) =
      [ InfoMsg $ "create direcotory " ++ path
      , ShellCommand $ "mkdir -p " ++ path ]


notexists :: (Revertable a ) => File -> a -> [ Command ]
notexists file revertable =
  if (isRevert revertable) then
    exists file (toggleRevert revertable) 
  else
    doNotExists file
  where
    doNotExists (File path) =
      [ IfCommand (BoolCommand $ "-e " ++ path)
        [ShellCommand $ "rm -f " ++ path ]
        [InfoMsg $ path ++ " does not exists"]
      ]
    doNotExists (Symlink path target) = doNotExists $ File path
    doNotExists  (Directory path) =
      [ IfCommand (BoolCommand $ "-e " ++ path)
        [ShellCommand $ "rm -fr " ++ path ]
        [InfoMsg $ path ++ " does not exists"]
      ]



-- ------------------------------------------------------------
--
-- File Content
--
-- ------------------------------------------------------------

contains :: (Revertable a ) => File -> [ String ] -> a -> [ Command ]
contains file content revertable =
  if (isRevert revertable) then
    notexists file (toggleRevert revertable) 
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
