{-|

Module      : Azubi.Syntax
Description : Basic Azubi syntax to combine 'State's
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

Functions to create a readable and typesafe
language, to describe system states.

-}
module Azubi.Syntax where

import           Azubi.Core.Model
import           System.FilePath.Posix

-- todo : Syntax belongs to core
{-|

Create a state that ensures that a file in @Path@ will
have the content of the String List.

Every String in the List will be a Line in the File.

-}
content :: String -> [String] -> State
content path contentToSet =
  States
    [HasFileContent path contentToSet]
    [ folderExists (takeDirectory path)
    , State [Not $ FolderExists path] [Remove path] Nothing
    , State [SkipChecks] [FileContent path contentToSet] Nothing
    ]
    (Just $ unwords ["Content for File", path])

{-|

Creates a state out of two states.
When the first State fails the second
State will not be checked nor enforced.

This can also be achieved using 'submodule':

@
stateA `requires` stateB == 'submodule' [stateB, stateA]
@

-}
requires :: State -> State -> State
stateA `requires` stateB = States [SkipChecks] [stateB, stateA] Nothing

{-|

Create a state containing of sub-states,
which have to be fulfilled in order.

If one state is not fulfilled, the following
will be ignored.

-}
submodule :: [State] -> State
submodule dependingStates = States [SkipChecks] dependingStates Nothing

{-|

make sure a folder exists

-}
folderExists :: Path -> State
folderExists path =
  let folders = reverse $ allFolders $ reverse $ splitDirectories path
  in States
       [FolderExists path]
       (map
          (\folder ->
             States
               [FolderExists folder]
               [ State
                   [Not $ DoesExist folder]
                   [Remove folder]
                   (Just $ "delete folder : " ++ folder)
               , State
                   [SkipChecks]
                   [CreateFolder folder]
                   (Just $ "create " ++ folder)
               ]
               Nothing)
          folders)
       Nothing
  where
    allFolders []     = []
    allFolders ["/"]  = []
    allFolders ["~"]  = []
    allFolders (x:xs) = joinPath (reverse $ x : xs) : allFolders xs

{-|

ensure there is a link file -> target

example:

@
link "~\/file" "~\/target"
@

will create a link at @~\/file\/@ pointing to @~\/target\/@

-}
link :: String -> String -> State
link path target =
  States
    [SymlinkExists path target]
    [ State [Not $ DoesExist path] [Remove path] Nothing
    , State [SkipChecks] [CreateSymlink path target] Nothing
    ]
    (Just $ "link " ++ path ++ " -> " ++ target)

{-|

'State' combinator.

@
[]
& folderExists "\/tmp\/foo"
& folderExists "\/tmp\/bar"
@

-}
(&) :: [State] -> State -> [State]
previouseStates & state = previouseStates ++ [state]
-- (!) :: [State] -> State -> [State]
-- states ! state = states ++ [(revertState state)]
