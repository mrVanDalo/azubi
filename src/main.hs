
import Azubi.Model
import Azubi.Boot
import Azubi.StateExecutor
import Azubi.Syntax
import Azubi.Installable


main :: IO ()
main = defaultMain $ []
  & content "/dev/shm/azubi.test" [ "Das ist ein"
                                  , "Azubi Test"
                                  ]
  & installed (Ebuild "vim")
  & uptodate (Ebuild "dev-lang/go")
  & installed (Ebuild "emacs")
  & installed (Git "git@github.com:mrVanDalo/azubi.git" "/dev/shm/azubi")







