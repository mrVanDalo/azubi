
module Azubi(azubiMain
            , AzubiConfig
            , azubi
            , (&)
            , (!)
            , (&?)
            , (!?)
            , submodule
            , requires
            , Gentoo(..)
            , installed
            , exists
            , contains
            , File(..)
            , azubiLogo
            ) where

import Azubi.Core.Syntax
import Azubi.Core.Command
import Azubi.Core.Provision

import Azubi.Systems.Gentoo

import Azubi.Render.BashScript
import Azubi.Render.Warez

import Azubi.Commands.Install
import Azubi.Commands.File

type AzubiConfig = [Command]

-- | main class called by the user
azubiMain :: [Command] -> IO ()
azubiMain config = azubiMainExecute azubiContext  config
  where
    azubiContext = (BashScript $ User None) -- | will be read from the command line

azubiMainExecute :: RenderContext -> [Command] -> IO()
azubiMainExecute (BashScript user) commands =
  writeFile "./azubi.sh" $ bashScriptExecuter user commands

azubiMainExecute render _ = putStrLn $ (show render) ++ "not supported yet"
