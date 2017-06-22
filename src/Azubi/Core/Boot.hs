{-|

Module      : Azubi.Core.Boot
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
module Azubi.Core.Boot where

import           Azubi.Core.Model
import           Azubi.Core.Interpreter
import           Azubi.Core.Interpreter.LocalUnixInterpreter hiding ( runCommand )
import           Options

{-|
  The function you should use to get you commands running.
-}
azubiMain :: [State] -> IO ()
azubiMain states = do
  context <- runCommand extractConfiguration
  execute (config context) states
  where
    config context = check (optSystem context)
      where
        check Gentoo = LocalContext $ UnixSystem verbosity
        check Debian = LocalContext $ UnixSystem verbosity
        check Ubuntu = LocalContext $ UnixSystem verbosity
        verbosity =
          if optVerbose context
            then Verbose
            else Silent

extractConfiguration :: AzubiOptions -> [String] -> IO AzubiOptions
extractConfiguration opts _ = return opts

data AzubiOptions = AzubiOptions
  { optVerbose :: Bool
  , optSystem  :: System
  }

instance Options AzubiOptions where
  defineOptions =
    pure AzubiOptions <*>
    simpleOption "verbose" False "shows the commands output" <*>
    defineOption (optionType_enum "Target System") systemConfig

descriptionHelper :: (Show a) => [String] -> [a] -> String
descriptionHelper text (x:xs) =
  unwords $
  text ++ ["[" ++ foldl (\a b -> a ++ ", " ++ show b) (show x) xs ++ "]"]
descriptionHelper text [] = unwords text

data System
  = Gentoo
  | Debian
  | Ubuntu
  deriving (Show, Eq, Bounded, Enum)

systemConfig :: Option System -> Option System
systemConfig opt =
  opt
  { optionLongFlags = ["system"]
  , optionDefault = Gentoo
  , optionDescription =
      descriptionHelper
        [ "The System type the States should"
        , "be enforced on. Different Systems have"
        , "different commands"
        ]
        [Gentoo ..]
  }
