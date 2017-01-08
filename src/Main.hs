

import Commands.Install
import Commands.File

import Systems.Gentoo

import Executer.BashScript

import Core.Syntax

-- import Executer.Dockerfile

main :: IO ()
main = do
  putStrLn $ bashScriptExecuter commands
    where
      commands = (azubi Gentoo)
        & installed "vim"
        ! installed "salt"
        ! exists (Directory "/dev/shm/azubi-test")
        & contains (File "/dev/shm/azubi/contains") [ "this is a azubi test-line : " ++ (show i) | i <- [1,2..10]]
        & exists (Symlink "/dev/shm/azubi-link" "/dev/shm/azubi/contains")
        ! exists (Directory "/dev/shm/Downloads")
        ! exists (Directory "/dev/shm/Documents")



-- next :
-- ------
-- dependencies
-- modules
-- commands
-- git

-- sketch on the syntax
--
-- main = do
--  azubi "workhorse" on Gentoo
--      & User.exists "palo" [ Uid 1000, Gid 1000 ]
--      & User.exists "renoise" [ Uid 1001, Home "/home/music" ]
--      & running "service"
--      & ip "192.1.2.2"
--      & uptodate (Git "/home/palo/.dot_i3/" "git@github.com/mrVanDalo/dot_i3" "master")                           -- | does pulls
--      & uptodate (GitWithSubmodules "/home/palo/.dot_jack" "git@github.com/mrVanDalo/dot_jack" "master")          -- | same but also takes car of submodules
--      & published (Git "/home/palo/.dot_shell" "git@github.com/mrVanDalo/dot_shell" "master" "azubi wip message")  -- | creates commits and pushes on the master branch using default message
--
