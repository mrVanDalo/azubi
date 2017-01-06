{-# LANGUAGE ScopedTypeVariables #-}

module Commands.Install where 

import Commands.Command

type Package = String

-- check if installed or not
class Installed a where
    installed :: a -> Package -> [ Command ]
    installed system package =
        [ InfoMsg $ "Check for " ++ package ++ " installation"
          , IfCommand 
            (isInstalled  system package)
            [ InfoMsg $ package ++ " is already installed" ] 
            [ InfoMsg $ "installing " ++ package
            , doInstall  system package ]
        ]


    --
    -- exit code of the command decides if it is True or False
    --
    isInstalled :: a -> Package -> BoolCommand

    --
    -- Do the installation
    --
    doInstall   :: a -> Package -> Command

    --
    -- Do the uninstallation
    --
    doUnInstall :: a -> Package -> Command



