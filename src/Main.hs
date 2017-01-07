

import qualified Commands.Install as Pkg
import Systems.Gentoo
import Systems.Debian
import Executer.BashScript
-- import Executer.Dockerfile

main :: IO ()
main = newmain



oldmain :: IO ()
oldmain = do
    putStrLn $ show $ Pkg.installed Gentoo  "vim"
    putStrLn $ show $ Pkg.installed Debian  "vim"

newmain :: IO ()
newmain = do
    putStrLn $ bashScriptExecuter $ concat [
        Pkg.installed Gentoo "vim"
        , Pkg.installed Gentoo "salt"
        , Pkg.installed Gentoo "wireshark"
        ]


-- main = do
--  azubi "workhorse" on Gentoo
--      & Pkg.installed [ "vim", "salt", "wireshark" ]
--      & File.exists [ "/home/palo" ]
--      & User.exists "palo" [ Uid 1000, Gid 1000 ]
--      & User.exists "renoise" [ Uid 1001, Home "/home/music" ]
--
