{-# LANGUAGE ScopedTypeVariables #-}

module Systems.Gentoo where

import Commands.Install
import Commands.Command

data Gentoo = Gentoo

instance Installed Gentoo where
    isInstalled Gentoo package = BoolCommand $
        "test `eix -e " ++ package ++ " | head -n1 | cut -d' ' -f 1` == '[I]'"
    doInstall   Gentoo package = ShellCommand $ "emerge " ++ package
    doUnInstall Gentoo package = ShellCommand $ "emerge --unmerge " ++ package

