
module Azubi.Model where


type Path = String
type Target = String
type Arguments = String
type Comment = String

-- | command args -> exit code
data Command = Run String [Arguments] (Maybe Comment)
             | FileContent Path [String]
             | CreateSymlink Path Target
             | CreateFolder Path
             deriving (Show, Eq)

data CommandResult = Success
                   | Failure
                   deriving (Show, Enum, Eq)


-- | command args -> Positive/success/failure
data Check = Check String [Arguments] (Maybe Comment)
           | HasFileContent Path [String]
           | SymlinkExists Path Target
           | FolderExists Path
             deriving (Show, Eq)

data CheckResult = Yes
                 | No
                 deriving (Show, Enum, Eq)

-- | state -> fulfilled/unfulfilled
data State =

  -- | State contains checks and commands
  -- if one command failed
  -- the following commands will not be
  -- executed.
  State [Check] [Command] (Maybe Comment)

  -- | To create depended states
  -- which should stop being executed
  -- when a previous state fails
  | States [Check] [State] (Maybe Comment)
           deriving (Show, Eq)

data StateResult = Fulfilled
                 | Unfulfilled
                 deriving (Show, Eq, Enum)





