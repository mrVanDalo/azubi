{-|

Module      : Azubi.Installable
Description : provide install function
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

To install software on your computer.

-}
module Azubi.Installable where

import Azubi.Model
import Azubi.Runable

class Installable a where

  {-|

install a piece of software on the system.
The /piece/ of software should be typed of course.

-}
  installed :: a -> State



{-|

make sure something is installed and
up to date.

-}
class Updatable a where


  {-|

make sure something is installed and
up to date.

-}
  uptodate :: a -> State



{-|

Ebuild is a Portage package which can
be installed and or updated.

-}
data Ebuild = Ebuild String

instance Installable Ebuild where

  installed (Ebuild package) = State
                               [Check "eix" ["--exact", "--nocolor", "--installed", package] (Just $ "check if package " ++ package ++ " is installed")]
                               [Run "emerge" [package] (Just $ "installing " ++ package)]
                               Nothing

instance Updatable Ebuild where

  uptodate (Ebuild package) = States
                              [ installed (Ebuild package)
                                , State
                                  [Not $ Check "eix" ["--upgrade-", "--nocolor",  package] Nothing]
                                  [Run "emerge" [package] (Just $ "upgrade " ++ package)]
                                  Nothing
                              ]
                              Nothing







type RepoUrl = String

{-|

Git is something that can be installed or updated

-}
data Git = Git RepoUrl Path


instance Installable Git where

  installed (Git repository path) = State
                                    [FolderExists path]
                                    [Run "git"
                                      ["clone"
                                      , repository
                                      , path]
                                      (Just $ "cloning " ++ repository ++ " to " ++ path)]
                                    Nothing

instance Updatable Git where

  uptodate (Git repo path) =
    States
    [ installed (Git repo path )
    , run (Always "git" ["--work-tree=" ++ path, "pull"])
    ]
    Nothing


