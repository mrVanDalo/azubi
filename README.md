# Azubi

[![Build Status](https://travis-ci.org/mrVanDalo/azubi.svg?branch=master)](https://travis-ci.org/mrVanDalo/azubi)

Is a very simple DevOps tool, which will never "reach" enterprise level.

## Goals

* Readable -> Haskell
* Check your rule set before changing your system -> Strong Type-system of Haskell
* Adaptive, -> can run on all kinds of Linux and it is also planed to run on osx and Windows
* Lightweight -> No installation (except some basic shell tools) needed on the target host.

## Features

### Different types of Execution

You can 

* enforce everything by command line (not yet)
* create a bash script which you can run for system setup
* use ssh to setup a target host (not yet)
* create different configurations for different situations (not yet)
* export to a Dockerfile (not yet)
* export to a Bat file (not yet)

### How to start

Install `azubi` via cabal.

    cabal install azubi

create a file (e.g. `config.hs`) somewhere you like with the content

    #!/usr/bin/env runghc
    
    import Azubi
    
    main :: IO ()
    main = azubiMain $ azubiConfig Gentoo $ []
        & installed "vim"


call the script to get a help

    ./config.hs --help


call the script to get a bashscript

    ./config.hs --output "my-first-azubi-script.sh"


# Syntax




## Commands

Every Command should be revertable.

### installed

#### Package 

install vim if not already done:

    & installed "vim"

uninstall vim if vim is installed:

    ! installed "vim"

### exists

#### Files

create files, directories and symlinks : 

    & exists (File "~/.vimrc")
    & exists (Directory "~/.vim")
    & exists (Symlink ".bashrc" "~/.bashrc.d/bashrc")
    
delete files files, directories and symlinks : 

    & exists (File "~/.vimrc")
    & exists (Directory "~/.vim")
    & exists (Symlink ".bashrc" "~/.bashrc.d/bashrc")

#### Git projects

pull git repository if not pressent:

    & exists (Git "git@github.com:mrVanDalo/azubi.git" "~/develop/azubi" [Branch "develop"])

you can give it options

* Branch "branchname"
* Recursive
* more to follow ... 

## Logic Components


### `requires`


### `submodule`


### Combiner

#### &, !
#### !?&,!?!, &?&, &?!
