{-# LANGUAGE ScopedTypeVariables #-}

module Systems.Debian where 

import Commands.Install
import Commands.Command

data Debian = Debian

instance Installed Debian where
    isInstalled Debian package = BoolCommand $ "apt --check " ++ package
    doInstall   Debian package = ShellCommand $ "apt-get install " ++ package
    doUnInstall Debian package = ShellCommand $ "apt-get uninstall " ++ package

