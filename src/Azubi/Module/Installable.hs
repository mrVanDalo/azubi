{-|

Module      : Azubi.Module.Installable
Description : provide install function
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

To install software on your computer.

-}
module Azubi.Module.Installable where

import           Azubi.Core.Model

import           Azubi.Module.Runable

{-|

To install Software you have to
instance the Installable class.

-}
class Installable a
  {-|

install a piece of software on the system.
The /piece/ of software should be typed of course.

-}
   where
  installed :: a -> State

{-|

Same like 'Installable' but will also
make sure there you have the newest
version.

-}
class Updatable a
  {-|

make sure something is installed and
up to date.

-}
   where
  uptodate :: a -> State

{-|

Ebuild is a Portage package (used by Gentoo and Funtoo).

<http://gentoo.org>

See 'installed'.

-}
newtype Ebuild =
  Ebuild String

instance Installable Ebuild where
  installed (Ebuild package) =
    State
      [ Check
          "eix"
          ["--exact", "--nocolor", "--installed", package]
          (Just $ unwords ["check if package ", package, " is installed"])
      ]
      [Run "emerge" [package] (Just $ "installing " ++ package)]
      (Just $ "installed " ++ package)

instance Updatable Ebuild where
  uptodate (Ebuild package) =
    States
      [SkipChecks]
      [ installed (Ebuild package)
      , State
          [Not $ Check "eix" ["--upgrade-", "--nocolor", package] Nothing]
          [Run "emerge" [package] (Just $ "upgrade " ++ package)]
          Nothing
      ]
      (Just $ "up to date " ++ package)

{-|

Git is a version control system.

<http://git.scm.com>

See 'installed'.

-}
data Git = Git
  { gitRepositoryUrl    :: String
  , gitRepositoryPath   :: String
  , gitRepositoryOtions :: [GitOption]
  }

data GitOption =
  Recursive

instance Installable Git where
  installed (Git repoUrl repoPath repoOptions) =
    State
      [FolderExists repoPath]
      [ Run
          "git"
          (["clone", repoUrl, repoPath] ++ extractCloneOptions repoOptions)
          (Just $ "cloning " ++ repoUrl ++ " to " ++ repoPath)
      ]
      (Just $ "installed (git " ++ repoPath ++ " <- " ++ repoUrl ++ ")")
    where
      extractCloneOptions :: [GitOption] -> [String]
      extractCloneOptions [] = []
      extractCloneOptions (Recursive:xs) =
        "--recursive" : extractCloneOptions xs

instance Updatable Git where
  uptodate package =
    States
      [SkipChecks]
      [ installed package
      , run (Always "git" ["--work-tree=" ++ gitRepositoryPath package, "pull"])
      ]
      (Just $
       unwords
         [ "up to date (git "
         , gitRepositoryPath package
         , " <- "
         , gitRepositoryUrl package
         , ")"
         ])

{-|

Gem installer. The Ruby package manager

<https://rubygems.org/>

-}
data GemOption =
  User
  deriving (Enum, Show, Eq)

data Gem = Gem
  { gemPackageName :: String
  , gemOptions     :: [GemOption]
  }

instance Installable Gem where
  installed Gem {gemPackageName = package, gemOptions = options} =
    State
      [Check "gem" ["list", "-i", package] Nothing]
      [installCommand]
      (Just $ "installed gem " ++ package)
    where
      installCommand =
        if User `elem` options
          then Run "gem" ["install", "--user-install", package] Nothing
          else Run "gem" ["install", package] Nothing


 -- todo : add updatable as well
