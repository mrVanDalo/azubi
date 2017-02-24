{-|

Module      : Azubi.Boot
Description : Functions to create an Azubi Programm
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

delivers functions you need for starting the
whole azubi process and parsing options from
the command line.

-}
module Azubi.Boot(defaultMain) where

import Options
import Azubi.Model
import Azubi.StateExecutor
import Azubi.StateExecutors.LocalUnixStateExecutor


{-|
  The function you should use to get you commands running.
-}
defaultMain :: [State] -> IO ()
defaultMain states = do
  context <- runCommand extractConfiguration
  execute (config context) states
  where
    -- todo : also do dummy calls here
    config context = check (optSystem context)
      where
        check Gentoo = (LocalContext UnixSystem)
        check Debian = (LocalContext UnixSystem)
        check Ubuntu = (LocalContext UnixSystem)



extractConfiguration ::  AzubiOptions -> [String] -> IO AzubiOptions
extractConfiguration opts _ = return opts

data AzubiOptions = AzubiOptions { optExecution :: Execution
                                 , optSystem :: System
                                 }

instance Options AzubiOptions where
  defineOptions = pure AzubiOptions
    <*> defineOption (optionType_enum "Exec Type") execConfig
    <*> defineOption (optionType_enum "Target System") systemConfig

descriptionHelper :: (Show a ) => [String] -> [ a ] -> String
descriptionHelper text (x:xs)= unwords $ text ++ ["[" ++ foldl (\a b -> a ++ ", " ++ (show b)) (show x) xs ++ "]" ]
descriptionHelper text [] = unwords text


data Execution = Command
               | Dummy
               deriving (Show, Eq, Bounded, Enum)

execConfig :: Option Execution -> Option Execution
execConfig opt =
        opt{ optionLongFlags = ["exec"]
           , optionDefault = Command
           , optionDescription =
             descriptionHelper
             [ "Type of Execution"
             , "which is more or less the way the rules should"
             , "System States should be enforced."]
             [Command ..]
           }


data System = Gentoo
            | Debian
            | Ubuntu
               deriving (Show, Eq, Bounded, Enum)

systemConfig :: Option System -> Option System
systemConfig opt =
        opt{ optionLongFlags = ["system"]
           , optionDefault = Gentoo
           , optionDescription =
             descriptionHelper
             [ "The System type the States should"
             , "be enforced on. Different Systems have"
             , "different commands"]
             [Gentoo ..]
           }
