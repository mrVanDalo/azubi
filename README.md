# Azubi

[![Join the chat at https://gitter.im/azubi-configuration/Lobby](https://badges.gitter.im/azubi-configuration/Lobby.svg)](https://gitter.im/azubi-configuration/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/mrVanDalo/azubi.svg?branch=master)](https://travis-ci.org/mrVanDalo/azubi)
[![Version](https://img.shields.io/badge/version-0.1.0.1-green.svg)](https://github.com/mrVanDalo/azubi/releases/tag/0.1.0.1)
[![License](https://img.shields.io/badge/license-gpl-green.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

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

* *Branch "branchname"*
* *Recursive*
* more to follow ... 

### contains

To set the content of a file.

    & contains (File "~/.vimrc") [ "content of the file"
                                 , "should be here"
                                 , "in this string array" ]

works also with `Symlinks`.


## Logic Components


### Combiner

#### &, !

Almost every command should be revertable. So you have 2 states

* do it -> `&`
* undo it -> `!`

this ensures the file `/tmp/foo` exist

        & exists (File "/tmp/foo")

and this ensures the file does not exist

        ! exists (File "/tmp/foo")

In most cases it happens what you expect, but sometimes it's not so obvious so we it is written right next to the command.
(e.g.:) `! exists (File "/tmp/foo")` will delete a file `/tmp/foo` but won't delete a directory `/tmp/foo`

#### !?&,!?!, &?&, &?!

They are special cases of `!` and `&` and should be read like `X if in context Y` -> `X?Y` and start only to make 
sense in combination of submodules (see later).

* `&?&` is `&` if you are in a `do it` context.
* `!?&` is `!` if you are in a `do it` context.
* `&?!` is `&` if you are in a `undo it` context.
* `!?!` is `!` if you are in a `undo it` context.

for example 

    & (submodule $ []
      &?& contains (File "/dev/shm/test") ["text"]
      & exists (Symlink  "~/.vimrc" "/dev/shm/test")
    )

would be reverted like this

    ! (submodule $ []
      &?& contains (File "/dev/shm/test") ["text"]
      & exists (Symlink  "~/.vimrc" "/dev/shm/test")
    )

which is similar to 

    ! exists (Symlink  "~/.vimrc" "/dev/shm/test")


### submodule

to group a bunch of command together to on command you can negate all at once if you want.
But you can create a much more sophisticated combination of commands using
`!?&`,`!?!`,`&?&` and `&?!`.

for example 

    & (submodule $ []
      & contains (File "/dev/shm/test") ["text"]
      & exists (Symlink  "~/.vimrc" "/dev/shm/test")
    )

is equivalent to

    & contains (File "/dev/shm/test") ["text"]
    & exists (Symlink  "~/.vimrc" "/dev/shm/test")

but could be reverted like this

    ! (submodule $ []
      & contains (File "/dev/shm/test") ["text"]
      & exists (Symlink  "~/.vimrc" "/dev/shm/test")
    )

which is equivalent to

    ! contains (File "/dev/shm/test") ["text"]
    ! exists (Symlink  "~/.vimrc" "/dev/shm/test")

### requires

is used to create dependencies like "first do *this*, and when everything is fine do *this*".
They make most sense with submodules

    & ((submodule $ []
       & exists (Symlink "~/.vim"   "~/.dot_vim")
       & exists (Symlink "~/.vimrc" "~/.vim/vimrc")
      )
      `requires`
      (submodule $ []
       & exists (Git "git@github.com/myrepo/dot_vim.git" "~/.dot_vim" [Recursive])
      ))

If `requires` is called in a reverting context (e.g. using `!`) it will also create a dependency 
but twisted and the body will be reverted as well.

     ! ((submodule $ []
       & exists (Symlink "~/.vim"   "~/.dot_vim")
       & exists (Symlink "~/.vimrc" "~/.vim/vimrc")
      )
      `requires`
      (submodule $ []
       & exists (Git "git@github.com/myrepo/dot_vim.git" "~/.dot_vim" [Recursive])
      ))

is equivalent to

     & ((submodule $ []
       ! exists (Git "git@github.com/myrepo/dot_vim.git" "~/.dot_vim" [Recursive])
      )
      `requires`
      (submodule $ []
       ! exists (Symlink "~/.vim"   "~/.dot_vim")
       ! exists (Symlink "~/.vimrc" "~/.vim/vimrc")
      ))

