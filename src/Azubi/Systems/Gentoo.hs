
module Azubi.Systems.Gentoo where

import Azubi.Core.Context
import Azubi.Core.Revertable
import Azubi.Core.Command

import Azubi.Commands.Install

data Gentoo = Gentoo
            | GentooRevert

instance Context Gentoo where
  label _ = "Gentoo system"

instance Revertable Gentoo where
  isRevert GentooRevert = True
  isRevert Gentoo       = False

  toggleRevert GentooRevert = Gentoo
  toggleRevert Gentoo       = GentooRevert

instance Installed Gentoo where
    isInstalled _ package = BoolCommand $
        "`eix -e " ++ package ++ " | head -n1 | cut -d' ' -f 1` == '[I]'"
    doInstall   _ package = [ SuperUserShellCommand $ "emerge " ++ package ]
    doUnInstall _ package = [ SuperUserShellCommand $ "emerge --unmerge " ++ package ]

