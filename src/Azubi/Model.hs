
{-|

Module      : Azubi.Model
Description : Core low level Model of Azubi
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

This is the lowest level of Azubi.
It should deliver only the bare
minimum to create 'State' evaluation
and 'State' enforcement.

The 'State' object is the element
a user should use to formulate more complex
situations.

The idea here is to create a simple AST which
can be run on different setups.

* export to Script
* run on the Machine (Linux, OSX and (maybe) Windows)
* run over SSH on another computer

-}
module Azubi.Model where


type Path = String
type Target = String
type Argument = String
type Comment = String

{-|

A command is something that will be run.
In a normal case you put a 'Check' before
using a 'State'

When a Command get executed,
it will create a 'CommandResult'.

@ command -> exit code -> 'CommandResult' @

-}
data Command = Run String [Argument] (Maybe Comment)
             | FileContent Path [String]
             | CreateSymlink Path Target
             | CreateFolder Path
             deriving (Show, Eq)


{-|

The Result of a 'Command' will be matched
to a 'CommandResult'. The basic rule is

@
0 -> 'Success'
_ -> 'Failure'
@

-}
data CommandResult = Success
                   | Failure
                   deriving (Show, Enum, Eq)


{-|

Gather information about a situation.

@
check -> success/failure
check -> exit code -> success/failure
@

-}
data Check =
  {-|

Check if command returns exit status

@
0 -> Yes
_ -> No
@

-}
  Check String [Argument] (Maybe Comment)
  | AlwaysYes
  -- | Opposite result of a 'Check'
  | Not Check
  -- | Check if 'Path' has content
  | HasFileContent Path [String]
  -- | Check if a Symbolic link exists to a specific target
  | SymlinkExists Path Target
  -- | Check if a folder exists
  | FolderExists Path
           deriving (Show, Eq)

{-|

The result of a 'Check'.

-}
data CheckResult = Yes
                 | No
                 deriving (Show, Enum, Eq)

{-|

The low level element to formulate a state on a machine.
If the check returns 'No' the 'Command's will run.
So the author should make sure the 'Command' will result
in a 'Yes' the next time this state will run.

All 'Check's have to return 'Yes' to avoid the 'Command's
to be run.

If a 'Command' returns a 'Failure' the following commands
will not be called.
Same holdes for 'States' if one of the states fail,
the following 'State's will not be /run/.

-}
data State =

  -- | State contains checks and commands
  -- if one command failed
  -- the following commands will not be
  -- executed.
  State [Check] [Command] (Maybe Comment)

  -- | To create depended states
  -- which should stop being executed
  -- when a previous state fails
  | States [State] (Maybe Comment)
           deriving (Show, Eq)


{-|

When a 'State' is /run/ it returns one of the 'StateResult's.

* positive 'CheckResult's will result in 'Fulfilled'.
* negative 'CheckResult's with positive 'CommandResult's will result in 'Fulfilled'.
* negative 'CheckResult's with negative 'CommandResult's will result in 'Unfulfilled'.

-}
data StateResult = Fulfilled
                 | Unfulfilled
                 deriving (Show, Eq, Enum)





