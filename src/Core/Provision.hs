

module Core.Provision where


data RenderContext = User SuperUserMethod
                   | Root

-- | the way SuperUserCommands should be called
data SuperUserMethod = Sudo
                     | Su
                     | None


