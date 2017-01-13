

module Azubi.Core.Provision where

-- | what type of user is running the script
data UserContext = User SuperUserMethod
                 | Root
                 deriving(Show)

-- | the way SuperUserCommands should be called
data SuperUserMethod = Sudo
                     | Su
                     | None
                   deriving(Show)

type OutputFile = String

-- | different output which can be produced
data  RenderContext = BashScript UserContext OutputFile
                    | Dockerfile
                   deriving(Show)




