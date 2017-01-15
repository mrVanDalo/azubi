
module Azubi(azubiMain
            , AzubiConfig
            , azubiConfig
            , (&)
            , (!)
            , (&?&)
            , (!?&)
            , (&?!)
            , (!?!)
            , submodule
            , requires
            , Gentoo(..)
            , installed
            , exists
            , contains
            , File(..)
            , Repository(..)
            , azubiLogo
            , run
            ) where

import Options


import Azubi.Core.Syntax
import Azubi.Core.Command
import Azubi.Core.Provision

import Azubi.Systems.Gentoo

import Azubi.Render.BashScript
import Azubi.Render.Warez

import Azubi.Commands.Install
import Azubi.Commands.Existance
import Azubi.Commands.File
import Azubi.Commands.Git
import Azubi.Commands.Run

type AzubiConfig = [Command]

-- | main class called by the user

azubiMain :: [Command] -> IO ()
azubiMain config =
  do
    renderContext <- runCommand extractArguments
    azubiMainExecute renderContext config

azubiMainExecute :: RenderContext -> [Command] -> IO()
azubiMainExecute (BashScript user output) commands =
  writeFile output $ bashScriptExecuter user commands

azubiMainExecute render _ = putStrLn $ (show render) ++ "not supported yet"


-- | option parsing

extractArguments :: AzubiOptions -> [String] -> IO RenderContext
extractArguments opts args =
  return (BashScript ( User None ) (optOutput opts))

data AzubiOptions = AzubiOptions { optInit :: Bool
                                 , optQuiet :: Bool
                                 , optOutput :: String}

instance Options AzubiOptions where
  defineOptions = pure AzubiOptions
    <*> simpleOption "init" False
    "create init project in home folder"
    <*> simpleOption "quiet" False
    "Whether to be quiet."
    <*> simpleOption "output" "./azubi.sh"
    "Outfile of the scritpt"
