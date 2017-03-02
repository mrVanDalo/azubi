# Azubi

[![Build Status](https://travis-ci.org/mrVanDalo/azubi.svg?branch=master)](https://travis-ci.org/mrVanDalo/azubi)
[![Version](https://img.shields.io/badge/version-0.2.0.1-green.svg)](https://github.com/mrVanDalo/azubi/releases/tag/0.2.0.1)
[![License](https://img.shields.io/badge/license-gpl-green.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Join the chat at https://gitter.im/azubi-configuration/Lobby](https://badges.gitter.im/azubi-configuration/Lobby.svg)](https://gitter.im/azubi-configuration/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Is a very simple DevOps tool, which will never "reach" enterprise level.

## Goals

* Readable -> Haskell
* Check your rule set before changing your system -> Strong Type-system of Haskell
* Adaptive -> can run on all kinds of Linux and it is also planed to run on osx and Windows
* Lightweight -> No installation (except some basic shell tools) needed on the target host.


## How to start

Install `azubi` via cabal.

    cabal install azubi

create a file (e.g. `config.hs`) somewhere you like with the content

    #!/usr/bin/env runghc
    
    import Azubi
    
    main :: IO ()
    main = azubiMain $ []
           & installed (Ebuild "vim")
           & uptodate (Git "git@github.com:mrVanDalo/azubi.git" "/dev/shm/azubi")
           & installed (Git "git@github.com:mrVanDalo/azubi-config.git" "/dev/shm/azubi-config")
           & run (Always "touch" ["/dev/shm/run.test"])
           & link "/dev/shm/azubi.link" "/dev/shm/azubi"


call the script to get a help

    ./config.hs --help


## Links

* [Hackage Documentation](http://hackage.haskell.org/package/azubi)
