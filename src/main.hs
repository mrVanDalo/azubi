import           Azubi

main :: IO ()
main =
  defaultMain $
  [] & content "/dev/shm/azubi.test" ["Das ist ein", "Azubi Test"] &
  installed (Ebuild "vim") &
  uptodate (Ebuild "dev-lang/go") &
  installed (Ebuild "emacs") &
  uptodate (Git "git@github.com:mrVanDalo/azubi.git" "/dev/shm/azubi") &
  installed
    (Git "git@github.com:mrVanDalo/azubi-config.git" "/dev/shm/azubi-config") &
  run (Always "echo" ["hallo"]) &
  run (Once "echo" ["penis"] "/dev/shm/.help") &
  link "/dev/shm/azubi.link" "/dev/shm/azubi"
