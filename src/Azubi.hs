{-|

Module      : Azubi
Description : Azubi main class is all you need.
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

Example:

@
import Azubi

main :: IO ()
main = azubiMain $ []
       & installed (Ebuild "vim")
       & uptodate (Git "git@github.com:mrVanDalo\/azubi.git" "\/dev\/shm\/azubi")
       & installed (Git "git@github.com:mrVanDalo\/azubi-config.git" "\/dev\/shm\/azubi-config")
       & run (Always "touch" ["/dev/shm/run.test"])
       & link "\/dev\/shm\/azubi.link" "\/dev\/shm\/azubi"
@

-}
module Azubi ( State(..)
             , Ebuild(..)
             , Git(..)
             , GitOption(..)
             , RunCommand(..)
             , installed
             , Installable
             , uptodate
             , Updatable
             , run
             , link
             , folderExists
             , content
             , requires
             , submodule
             , (&)
             , azubiMain
             ) where


import Azubi.Core.Model
import Azubi.Core.Boot
import Azubi.Syntax
import Azubi.Module.Runable
import Azubi.Module.Installable
