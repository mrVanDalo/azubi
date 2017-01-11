
module Azubi.Core.Command where

data Command = ShellCommand String
             | SuperUserShellCommand String
             | IfCommand { testCommand :: BoolCommand
                         , thenCommand :: [ Command ]
                         , elseCommand :: [ Command ]
                         }
             | FileContent { path :: String
                           , content :: [ String ]
                           }
             | InfoMsg String
             | ErrorMsg String
             | Dependency { body :: [Command]
                          , dependency :: [Command] }
             | LogMsg String
             deriving (Show, Eq)

-- exit code will decide if if is True or False
data BoolCommand = BoolCommand String
    deriving (Show, Eq)

