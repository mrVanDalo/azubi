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

import Azubi.Core.Model

import Azubi.Module.Runable



{-|

To install Software you have to
instance the Installable class.

-}
class Installable a where

  {-|

install a piece of software on the system.
The /piece/ of software should be typed of course.

-}
  installed :: a -> State



{-|

Same like 'Installable' but will also
make sure there you have the newest
version.

-}
class Updatable a where


  {-|

make sure something is installed and
up to date.

-}
  uptodate :: a -> State



{-|

Ebuild is a Portage package (used by Gentoo and Funtoo).

<http://gentoo.org>

See 'installed'.

-}
data Ebuild = Ebuild String

instance Installable Ebuild where

  installed (Ebuild package) = State
                               [Check "eix" ["--exact", "--nocolor", "--installed", package] (Just $ "check if package " ++ package ++ " is installed")]
                               [Run "emerge" [package] (Just $ "installing " ++ package)]
                               Nothing

instance Updatable Ebuild where

  uptodate (Ebuild package) = States [AlwaysYes]
                              [ installed (Ebuild package)
                                , State
                                  [Not $ Check "eix" ["--upgrade-", "--nocolor",  package] Nothing]
                                  [Run "emerge" [package] (Just $ "upgrade " ++ package)]
                                  Nothing
                              ]
                              Nothing






-- | Url of the repository used by
-- git clone
type RepoUrl = String

{-|

Git is a version control system.

<http://git.scm.com>

See 'installed'.

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
    States [AlwaysYes]
    [ installed (Git repo path )
    , run (Always "git" ["--work-tree=" ++ path, "pull"])
    ]
    Nothing


