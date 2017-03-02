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

import Azubi.Core.Model


{-|

Create a state that ensures that a file in @Path@ will
have the content of the String List.

Every String in the List will be a Line in the File.

-}
content :: Path -> [String] -> State
content path fileContent = State [HasFileContent path fileContent]
                                 [FileContent path fileContent]
                                 (Just $ unwords [ "Content for File" , path ])

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
stateA  `requires` stateB = States [Not AlwaysYes] [stateB, stateA] Nothing

{-|

Create a state containing of sub-states,
which have to be fulfilled in order.

If one state is not fulfilled, the following
will be ignored.

-}
submodule :: [State] -> State
submodule states = States [Not AlwaysYes] states Nothing


{-|

make sure a folder exists

-}
folderExists :: Path -> State
folderExists path = State [FolderExists path] [CreateFolder path] Nothing

{-|

ensure there is a link file -> target

example:

@
link "~\/file" "~\/target"
@

will create a link at @~\/file\/@ pointing to @~\/target\/@

-}
link :: Path -> Path -> State
link path target =
  States
  [ SymlinkExists path target ]
  [ State [Not $ DoesExist path] [Remove path] Nothing
  , State [Not AlwaysYes] [CreateSymlink path target] Nothing
  ]
  (Just $ "link " ++ path ++ " to " ++ target)

{-|

'State' combinator.

@
[]
& folderExists "\/tmp\/foo"
& folderExists "\/tmp\/bar"
@

-}
(&) :: [State] -> State -> [State]
states & state = states ++ [state]


-- (!) :: [State] -> State -> [State]
-- states ! state = states ++ [(revertState state)]

