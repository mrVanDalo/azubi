{-# LANGUAGE ScopedTypeVariables #-}

module Commands.Install where

import Core.Context
import Core.Revertable

import Core.Command

type Package = String

-- check if installed or not
class (Context a, Revertable a) => Installed a where

    installed :: Package -> a -> [ Command ]
    installed package context =
      if (isRevert context) then
        uninstallCommands
      else
        installCommands
      where
        uninstallCommands =
          [ IfCommand (isInstalled  context package)
            ([ InfoMsg $ "uninstalling " ++ package ] ++ (doUnInstall  context package))
            [ InfoMsg $ package ++ " is not installed" ]
          ]
        installCommands =
          [ IfCommand (isInstalled  context package)
            [ InfoMsg $ package ++ " is already installed" ]
            $ [ InfoMsg $ "installing " ++ package ] ++ doInstall  context package
          ]


    --
    -- exit code of the command decides if it is True or False
    --
    isInstalled :: a -> Package -> BoolCommand

    doInstall   :: a -> Package -> [ Command ]

    doUnInstall :: a -> Package -> [ Command ]


