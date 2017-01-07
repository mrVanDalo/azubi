
module Commands.Command where

data Command =
        ShellCommand String
        | IfCommand {
            testCommand :: BoolCommand
            , thenCommand :: [ Command ]
            , elseCommand :: [ Command ]
            }
        | InfoMsg String
        | ErrorMsg String
    deriving (Show)

-- exit code will decide if if is True or False
data BoolCommand = BoolCommand String
    deriving (Show)
