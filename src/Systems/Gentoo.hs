{-# LANGUAGE ScopedTypeVariables #-}

module Systems.Gentoo where

import Core.Context
import Core.Revertable

import Commands.Install
import Core.Command

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

