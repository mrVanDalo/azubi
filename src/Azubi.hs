
module Azubi( azubiMain
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
            , GitOptions(..)
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
azubiMainExecute (BashScript user output) commands = do 
  putStr $ unlines bashScriptHeader
  writeFile output $ bashScriptExecuter user commands
  where
    bashScriptHeader = [ "Generating Bashscript"
                       , "---------------------"
                       , "output : " ++ output
                       , "user mode : " ++ (userMode user)
                       ]
    userMode (User None) = "login user with no root command strategy"
    userMode (User Su) = "login user with su as root command strategy"
    userMode (User Sudo) = "login user with sudo as root command strategy"
    userMode Root = "root"


azubiMainExecute render _ = putStrLn $ (show render) ++ "not supported yet"


-- | option parsing

extractArguments :: AzubiOptions -> [String] -> IO RenderContext
extractArguments opts args =
  return (BashScript (userMode $ optUser opts) (optOutput opts))
  where
    userMode RootConfiguration = Root
    userMode UserNoneConfiguration = User None
    userMode UserSuConfiguration = User Su
    userMode UserSudoConfiguration = User Sudo




data AzubiOptions = AzubiOptions { optOutput :: String
                                 , optUser :: UserConfiguration
                                 }

instance Options AzubiOptions where
  defineOptions = pure AzubiOptions
    <*> simpleOption "output" "./azubi.sh"
    "Outfile of the scritpt"
    <*> defineOption (optionType_enum "User Type") userConfiguration



-- | UserContext parsing (see "User Type" parameter)
-- | -----------------------------------------------
data UserConfiguration = RootConfiguration
                       | UserNoneConfiguration
                       | UserSuConfiguration
                       | UserSudoConfiguration
                       deriving (Bounded, Enum)

instance Show UserConfiguration where
  show RootConfiguration = "root"
  show UserNoneConfiguration = "user"
  show UserSudoConfiguration = "sudoUser"
  show UserSuConfiguration = "suUser"

userConfiguration :: Option UserConfiguration -> Option UserConfiguration
userConfiguration opt =
        opt{ optionLongFlags = ["user"]
           , optionDefault = UserNoneConfiguration
           , optionDescription =
             unwords $ [ "User type the commands should be run in."
                       , "For example 'UserSu' will run every command"
                       , "as normal login user, but the superuser commands via"
                       , "su -c 'command' so you have to enter the"
                       , "superuser password for every install command for example."
                       , "possible options are:"
                       ]
             ++ (map show [RootConfiguration ..])
           }
