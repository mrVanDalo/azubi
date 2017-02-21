#!/usr/bin/env runghc

-- | States are system states
-- |
-- | They are the core thing we have to describe a system
-- |
module Azubi.State where

import Options


-- | ------------------------------------------------------------
-- |
-- |
-- |                                                   modeling
-- |
-- |
-- | ------------------------------------------------------------



-- | System : Gentoo
-- | Execution : BashScript

type BashScriptOutput = String

data GentooBashScrip = GentooBashScrip BashScriptOutput

instance SystemConfiguration GentooBashScrip where

  header (GentooBashScrip output) = do
    putStrLn $ unlines [ "This setup is not implemented yet"
                       , "---------------------------------"
                       , "System : Gentoo"
                       , "execution : BashScript"
                       , "output : " ++ output
                       ]
    writeFile output $ unlines [ "#!/bin/bash" ]

  exec (GentooBashScrip output) (Installed (Package name)) = appendFile output $ unlines ["emerge " ++  name]
  exec (GentooBashScrip output) (Installed (RubyPackage name)) = appendFile output $ unlines ["gem install " ++  name]
  exec (GentooBashScrip output) (Installed (HaskellPackage name)) = appendFile output $ unlines [ "cabal install " ++  name ]

  exec (GentooBashScrip output) state = putStrLn $ "not supported yet : '" ++ (show state) ++ "'"


-- | System : Default
-- | Execution : Default

data DefaultSystemConfiguration = DefaultSystemConfiguration Execution System

instance SystemConfiguration DefaultSystemConfiguration where
  header (DefaultSystemConfiguration e system) = putStrLn $ unlines [ "This setup is not implemented yet"
                                                                    , "---------------------------------"
                                                                    , "System : " ++ (show system)
                                                                    , "execution : " ++ (show e)]

  -- exec (DefaultSystemConfiguration e system) state  = putStrLn $ show state



-- | ------------------------------------------------------------
-- |
-- |
-- |                                                main program
-- |
-- |
-- | ------------------------------------------------------------


main :: IO ()
main = defaultMain [ Installed $ Package "vim"
                   , Installed $ RubyPackage "Backup"
                   , Exists Folder
                   , Exists $ FileTarget File
                   , Exists $ FileTarget Symlink
                   ]



-- | ------------------------------------------------------------
-- |
-- |
-- |                                              option parsing
-- |
-- |
-- | ------------------------------------------------------------

defaultMain :: [State] -> IO ()
defaultMain states = do
  opts <- runCommand extractConfiguration
  callMainProgramm (optExecution opts) (optSystem opts) opts
  where
    callMainProgramm BashScript Gentoo opts = mainProgramm (GentooBashScrip $ optOutput opts) states
    callMainProgramm exec system opts = mainProgramm (DefaultSystemConfiguration exec system) states

extractConfiguration ::  AzubiOptions -> [String] -> IO AzubiOptions
extractConfiguration opts args = return opts

data AzubiOptions = AzubiOptions { optOutput :: String
                                 , optExecution:: Execution
                                 , optSystem :: System
                                 }

instance Options AzubiOptions where
  defineOptions = pure AzubiOptions
    <*> simpleOption "output" "./azubi.sh"
    "Outfile of the scritpt"
    <*> defineOption (optionType_enum "Exec Type") execConfig
    <*> defineOption (optionType_enum "Target System") systemConfig

execConfig :: Option Execution -> Option Execution
execConfig opt =
        opt{ optionLongFlags = ["exec"]
           , optionDefault = BashScript
           , optionDescription =
             descriptionHelper
             [ "Type of Execution"
             , "which is more or less the way the rules should"
             , "System States should be enforced."]
             [BashScript ..]
           }
descriptionHelper :: (Show a ) => [String] -> [ a ] -> String
descriptionHelper text (x:xs)= unwords $ text ++ ["[" ++ foldl (\a b -> a ++ ", " ++ (show b)) (show x) xs ++ "]" ]
descriptionHelper text [] = unwords text


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
