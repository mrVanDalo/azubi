

module TestContext where

import Azubi.Core.Command
import Azubi.Core.Context
import Azubi.Core.Revertable

import Azubi.Commands.Install

data TestContext = TestContext
                 | TestContextReverted
    deriving (Show, Eq)

instance Context TestContext where
  label _ = "test-context"

instance Revertable TestContext where
  isRevert TestContext = False
  isRevert TestContextReverted = True

  toggleRevert TestContext = TestContextReverted
  toggleRevert TestContextReverted = TestContext


instance Installed TestContext where
  isInstalled _ pkg = BoolCommand $ "check " ++ pkg
  doInstall _ pkg = [ SuperUserShellCommand $ "install " ++ pkg ]
  doUnInstall _ pkg = [ SuperUserShellCommand $ "uninstall " ++ pkg ]
