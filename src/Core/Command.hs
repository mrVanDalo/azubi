
module Core.Command where

-- todo :
-- need something like depends on.
--   Wenn Command A nicht erfolgreich war
--   Dann probier B erst gar nicht
--   => dafür müssen Commands Checkable sein.

data Command = ShellCommand String
             | IfCommand { testCommand :: BoolCommand
                         , thenCommand :: [ Command ]
                         , elseCommand :: [ Command ]
                         }
             | FileContent { path :: String
                           , content :: [ String ]
                           }
             | InfoMsg String
             | ErrorMsg String
             -- | todo : das soll noch rein
             -- | LogMsg String
             deriving (Show, Eq)

-- exit code will decide if if is True or False
data BoolCommand = BoolCommand String
    deriving (Show, Eq)

