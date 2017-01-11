module Azubi.Systems.Debian where

import Azubi.Core.Context
import Azubi.Core.Revertable
import Azubi.Core.Command

import Commands.Install

data Debian = Debian
            | DebianRevert

instance Context Debian where
  label _ = "Debian System"

instance Revertable Debian where
  isRevert Debian       = False
  isRevert DebianRevert = True

  toggleRevert Debian       = DebianRevert
  toggleRevert DebianRevert = Debian

instance Installed Debian where
    isInstalled _ package = BoolCommand $ "apt --check " ++ package
    doInstall   _ package = [ ShellCommand $ "apt-get install " ++ package ]
    doUnInstall _ package = [ ShellCommand $ "apt-get uninstall " ++ package ]

