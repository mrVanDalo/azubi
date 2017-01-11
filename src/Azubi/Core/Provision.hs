

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



-- | different output which can be produced
data  RenderContext = BashScript UserContext
                    | Dockerfile
                   deriving(Show)
